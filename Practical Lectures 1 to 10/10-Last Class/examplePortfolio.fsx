#r "nuget:FSharp.Data"
#r "nuget:NodaTime"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET, 2.0.0-beta9"
open System
open FSharp.Data
open NodaTime
open Plotly.NET
open FSharp.Stats
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))
fsi.AddPrinter<YearMonth>(fun ym -> $"{ym.Year}-{ym.Month}")

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

#load "Portfolio3.fsx"
open Portfolio
(**
### We will use the Common.fsx file
Make sure that this loads correctly.
*)
#load "../Common.fsx"
open Common

let [<Literal>] dataCache = __SOURCE_DIRECTORY__ + "/../data-cache/"
let [<Literal>] idAndReturnsFile = dataCache + "id_and_return_data.csv"
let [<Literal>] mySignalFile = dataCache + "qmj_growth.csv"

IO.File.ReadLines(idAndReturnsFile) |> Seq.truncate 5
IO.File.ReadLines(mySignalFile) |> Seq.truncate 5

type IdAndReturnCsv = CsvProvider<Sample=idAndReturnsFile,
                                  Schema="obsMain(string)->obsMain=bool,exchMain(string)->exchMain=bool",
                                  ResolutionFolder= __SOURCE_DIRECTORY__>

type Signal = CsvProvider<Sample=mySignalFile,
                          ResolutionFolder= __SOURCE_DIRECTORY__>

let idAndReturns = IdAndReturnCsv.Load(idAndReturnsFile)
let mySignal = Signal.Load(mySignalFile)

let rand = new Random()

let signalDistribution =
    mySignal.Rows
    |> Seq.choose(fun x -> x.Signal)
    |> Seq.sortBy(fun _ -> rand.NextDouble()) 
    |> Seq.truncate 1_000 
    |> Seq.toArray

let histogram =
    signalDistribution
    |> Chart.Histogram
    |> Chart.Show
(**
Index the data by security id and month.

- In this dataset, we'll use `row.Id` as the identifier. We'll assign it to
the `Other` SecurityId case, because it's a dataset specific one.
- In this dataset, the Eom variable defines the "end of month".
- The returns are for the month ending in EOM.
- The signals are "known" as of EOM. So you can use them on/after EOM. We'll
form portfolios in the month ending EOM; that's the `FormationMonth`.
*)
let msfBySecurityIdAndMonth =
    idAndReturns.Rows
    |> Seq.map(fun row -> 
        let index = Other row.Id, YearMonth(row.Eom.Year,row.Eom.Month)
        index, row)
    |> Map.ofSeq    

let signalBySecurityIdAndMonth =
    mySignal.Rows
    |> Seq.choose(fun row -> 
        // we'll use Seq.choose to drop the security if the security is missing. 
        match row.Signal with
        | None -> None // choose will drop these None observations
        | Some signal ->
            let index = Other row.Id, YearMonth(row.Eom.Year,row.Eom.Month)
            Some (index, signal) // choose will convert Some(index,signal) into
                                 // (index,signal) and keep that.
    )
    |> Map.ofSeq    
(**
The `securitiesByFormationMonth` that we'll use to define our investment universe.

*)
let securitiesByFormationMonth =
    idAndReturns.Rows
    |> Seq.groupBy(fun x -> YearMonth(x.Eom.Year, x.Eom.Month))
    |> Seq.map(fun (ym, xs) -> 
        ym, 
        xs 
        |> Seq.map(fun x -> Other x.Id) 
        |> Seq.toArray)
    |> Map.ofSeq

let getInvestmentUniverse formationMonth =
    match Map.tryFind formationMonth securitiesByFormationMonth with
    | Some securities -> 
        { FormationMonth = formationMonth 
          Securities = securities }
    | None -> failwith $"{formationMonth} is not in the date range"      
(**
Get signal.
*)
let getMySignal (securityId, formationMonth) =
    match Map.tryFind (securityId, formationMonth) signalBySecurityIdAndMonth with
    | None -> None
    | Some signal ->
        Some { SecurityId = securityId; Signal = signal }

let getMySignals (investmentUniverse: InvestmentUniverse) =
    let arrayOfSecuritySignals =
        investmentUniverse.Securities
        |> Array.choose(fun security -> 
            getMySignal (security, investmentUniverse.FormationMonth))    
    
    { FormationMonth = investmentUniverse.FormationMonth 
      Signals = arrayOfSecuritySignals }
(**
Get market capitalization
*)
let getMarketCap (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> None
    | Some row -> 
        match row.MarketEquity with
        | None -> None
        | Some me -> Some (security, me)
(**
Get returns.
*)
let getSecurityReturn (security, formationMonth) =
    // If the security has a missing return, assume that we got 0.0.
    // Note: If we were doing excess returns, we wouldd need 0.0 - rf.
    let missingReturn = 0.0
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> security, missingReturn
    | Some x ->  
        match x.Ret with 
        | None -> security, missingReturn
        | Some r -> security, r
(**
Restrictions. These come from the data documentation
section 1.2, "How to use the data". 
*)
let isObsMain (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.ObsMain

let isPrimarySecurity (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.PrimarySec

let isCommonStock (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.Common

let isExchMain (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.ExchMain

let hasMarketEquity (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.MarketEquity.IsSome

let myFilters securityAndFormationMonth =
    isObsMain securityAndFormationMonth &&
    isPrimarySecurity securityAndFormationMonth &&
    isCommonStock securityAndFormationMonth &&
    isExchMain securityAndFormationMonth &&
    isExchMain securityAndFormationMonth &&
    hasMarketEquity securityAndFormationMonth

let doMyFilters (universe:InvestmentUniverse) =
    let filtered = 
        universe.Securities
        // my filters expect security, formationMonth
        |> Array.map(fun security -> security, universe.FormationMonth)
        // do the filters
        |> Array.filter myFilters
        // now convert back from security, formationMonth -> security
        |> Array.map fst
    { universe with Securities = filtered }
(**
Define sample months
*)
let startSample = 
    idAndReturns.Rows
    |> Seq.map(fun row -> YearMonth(row.Eom.Year,row.Eom.Month))
    |> Seq.min

let endSample = 
    idAndReturns.Rows
    |> Seq.map(fun row -> YearMonth(row.Eom.Year,row.Eom.Month))
    |> Seq.max
    // The end of sample is the last month when we have returns.
    // So the last month when we can form portfolios is one month
    // before that.
    |> fun maxMonth -> maxMonth.PlusMonths(-1) 

let sampleMonths = 
    getSampleMonths (startSample, endSample)
    |> List.toArray
(**
Strategy function
*)
let formStrategy ym =
    ym
    |> getInvestmentUniverse
    |> doMyFilters
    |> getMySignals
    |> assignSignalSort "Mine" 3
    |> Array.map (giveValueWeights getMarketCap)
    |> Array.map (getPortfolioReturn getSecurityReturn)  
(**
Strategy portfolios 
*)
let portfolios =
    sampleMonths
    |> Array.Parallel.collect formStrategy
(**
Use the French data to get monthly risk-free rates.
*)
let ff3 = French.getFF3 Frequency.Monthly
let monthlyRiskFreeRate =
    ff3
    |> Array.map(fun x -> YearMonth(x.Date.Year,x.Date.Month),x.Rf)
    |> Map.ofArray
(**
Convert portfolios into excess returns.
*)
let portfolioExcessReturns =
    portfolios
    |> Array.Parallel.map(fun x -> 
        match Map.tryFind x.YearMonth monthlyRiskFreeRate with 
        | None -> failwith $"Can't find risk-free rate for {x.YearMonth}"
        | Some rf -> { x with Return = x.Return - rf })
(**
Plot the top portfolio
*)
let top =
    portfolioExcessReturns
    |> Array.filter(fun port ->
        port.PortfolioId = Indexed("Mine", 3))
    
let cumulateReturn (xs: PortfolioReturn array) =
    let mapper (priorRet:float) (thisObservation:PortfolioReturn) =
        let asOfNow = priorRet*(1.0 + thisObservation.Return)
        { thisObservation with Return = asOfNow}, asOfNow
    // remember to make sure that your sort order is correct.
    let sorted = xs |> Array.sortBy(fun x -> x.YearMonth)
    (1.0, sorted) 
    ||> Array.mapFold mapper 
    |> fst    

let topCumulative = top |> cumulateReturn
(**
Plotly.NET doesn't know about YearMonth, so we convert it to DateTime before plotting.
*)
let topCumulativeChart =
    topCumulative
    |> Array.map(fun x -> DateTime(x.YearMonth.Year,x.YearMonth.Month,1), x.Return)
    |> Chart.Line 
    |> Chart.withTitle "Growth of 1 Euro"

topCumulativeChart 
|> Chart.withTitle "Growth 1 Euro (Not Standerdized)"
|> Chart.Show
(**
Create one normalized to have 10\% annualized volatility
for the entire period. 
*)
let top10PctVol =
    let topAnnualizedVol = sqrt(12.0) * (top |> Seq.stDevBy(fun x -> x.Return))
    top 
    |> Array.map(fun x -> { x with Return = (0.1/topAnnualizedVol) * x.Return })
(**
Check to make sure it's 10\% vol. 
*)
sqrt(12.0) * (top10PctVol |> Seq.stDevBy(fun x -> x.Return)) 
(**
plot 
*)
let topNormalizedPlot =
    top10PctVol
    |> cumulateReturn
    |> Array.map(fun x -> DateTime(x.YearMonth.Year,x.YearMonth.Month,1), x.Return)
    |> Chart.Line 
    |> Chart.withTitle "Growth of 1 Euro"
    |> Chart.Show

let normalizeToTenPct (xs:PortfolioReturn array) =
    let annualizedVol = sqrt(12.0) * (xs |> Seq.stDevBy(fun x -> x.Return))
    xs 
    |> Array.map(fun x -> 
        { x with Return = (0.1/annualizedVol) * x.Return })
(**
Check 
*)
portfolioExcessReturns
|> Array.groupBy(fun port -> port.PortfolioId)
|> Array.map(fun (portId, xs) ->
    let normalized = xs |> normalizeToTenPct  
    portId,
    sqrt(12.0)*(normalized |> Seq.stDevBy(fun x -> x.Return)))

let portfolioReturnPlot (xs:PortfolioReturn array) =
    xs
    |> Array.map(fun x -> DateTime(x.YearMonth.Year,x.YearMonth.Month,1), x.Return)
    |> Chart.Line 
    |> Chart.withTitle "Growth of 1 Euro"

let topWithFunctionsPlot =
    top
    |> normalizeToTenPct
    |> cumulateReturn
    |> portfolioReturnPlot

(**
Joint plots
*)
let vwMktRf =
    let portfolioMonths = portfolioExcessReturns |> Array.map(fun x -> x.YearMonth)
    let minYm = portfolioMonths |> Array.min
    let maxYm = portfolioMonths |> Array.max
    
    ff3
    |> Array.map(fun x -> 
        { PortfolioId = Named("Mkt-Rf")
          YearMonth = YearMonth(x.Date.Year,x.Date.Month)
          Return = x.MktRf })
    |> Array.filter(fun x -> 
        x.YearMonth >= minYm &&
        x.YearMonth <= maxYm)

let combinedChart =
    Array.concat [portfolioExcessReturns; vwMktRf]
    |> Array.groupBy(fun x -> x.PortfolioId)
    |> Array.map(fun (portId, xs) ->
        xs
        |> normalizeToTenPct
        |> cumulateReturn
        |> portfolioReturnPlot
        |> Chart.withTraceName (portId.ToString()))
    |> Chart.Combine
    |> Chart.Show   
(**
Save results to a csv file.
*)
type PortfolioReturnCsv = CsvProvider<"portfolioName(string),index(int option),yearMonth(date),ret(float)">

let makePortfolioReturnCsvRow (row:PortfolioReturn) =
    let name, index =
        match row.PortfolioId with
        | Indexed(name, index) -> name, Some index
        | Named(name) -> name, None
    PortfolioReturnCsv
        .Row(portfolioName=name,
             index = index,
             yearMonth=DateTime(row.YearMonth.Year,row.YearMonth.Month,1),
             ret=row.Return)

portfolioExcessReturns
|> Array.map makePortfolioReturnCsvRow
|> fun rows -> 
    let csv = new PortfolioReturnCsv(rows)
    csv.Save("myExcessReturnPortfolios.csv")

