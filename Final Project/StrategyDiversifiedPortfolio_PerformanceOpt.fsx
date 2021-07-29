#r "nuget: FSharp.Stats, 0.4.1"
#r "nuget: FSharp.Data"

#load "../common.fsx"

open System
open FSharp.Data
open Common

open FSharp.Stats


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
(**
# Portfolio Optimization

We're now going to see how to do mean-variance portfolio optimization.
The objective is to find the portfolio with the greatest return per
unit of standard deviation.

In particular, we're going to identify the tangency portfolio. 
*)
type StockData =
    { Symbol : string 
      Date : DateTime
      Return : float }

// Get the Fama-French 3-Factor asset pricing model data.

let ff3 = French.getFF3 Frequency.Monthly

// Transform to a StockData record type.
let ff3StockData =
    [| 
       ff3 |> Array.map(fun x -> {Symbol="HML";Date=x.Date;Return=x.Hml})
       ff3 |> Array.map(fun x -> {Symbol="MktRf";Date=x.Date;Return=x.MktRf})
       ff3 |> Array.map(fun x -> {Symbol="Smb";Date=x.Date;Return=x.Smb})
    |] |> Array.concat

// Get factor data.

let myFactorPorts = CsvProvider<"myExcessReturnPortfolios.csv",
                                ResolutionFolder = __SOURCE_DIRECTORY__>.GetSample()

let long = 
    myFactorPorts.Rows 
    |> Seq.filter(fun row -> row.PortfolioName = "Mine" && row.Index = Some 3)
    |> Seq.map(fun x -> { Symbol = "Long"; Date = x.YearMonth; Return = x.Ret })
    |> Seq.toArray

let short = 
    myFactorPorts.Rows 
    |> Seq.filter(fun row -> row.PortfolioName = "Mine" && row.Index = Some 1)
    |> Seq.map(fun x -> { Symbol = "Short"; Date = x.YearMonth; Return = x.Ret})
    |> Seq.toArray

// type Return = { YearMonth : DateTime; Return : float }
let longShort =
    // this is joining long to short by YearMonth:DateTime
    let shortMap = short |> Seq.map(fun row -> row.Date, row) |> Map
    long
    |> Seq.map(fun longObs -> 
        match Map.tryFind longObs.Date shortMap with
        | None -> failwith "probably your date variables are not aligned"
        | Some shortObs -> { Symbol = "Long-Short"; Date = longObs.Date; Return = longObs.Return - shortObs.Return })
    |> Seq.toArray 

// Get Comparizon Investments
let vti = 
    "VTI"
    |> Tiingo.request
    |> Tiingo.startOn (DateTime(2000,1,1))
    |> Tiingo.getReturns

let bnd = 
    "BND"
    |> Tiingo.request
    |> Tiingo.startOn (DateTime(2000,1,1))
    |> Tiingo.getReturns
(**
These are daily returns. So let's convert them to monthly returns.

So let's combine the `VTI` and `BND` data, group by symbol and month,
and then convert to monthly returns.
*)
let standardInvestments =
    Array.concat [vti; bnd]
    |> Array.groupBy(fun x -> x.Symbol, x.Date.Year, x.Date.Month)
    |> Array.map(fun ((sym, year, month), xs) -> 
        let sortedRets = 
            xs
            |> Array.sortBy(fun x -> x.Date)
            |> Array.map(fun x -> x.Return)
        let monthlyGrossRet =
            (1.0, sortedRets)
            ||> Array.fold (fun acc x -> acc * (1.0 + x))
        { Symbol = sym
          Date = DateTime(year, month, 1)
          Return = monthlyGrossRet - 1.0 })

// Convert to excess returns
let rf = ff3 |> Seq.map(fun x -> x.Date, x.Rf) |> Map

let standardInvestmentsExcess =
    let maxff3Date = ff3 |> Array.map(fun x -> x.Date) |> Array.max
    standardInvestments
    |> Array.filter(fun x -> x.Date <= maxff3Date)
    |> Array.map(fun x -> 
        match Map.tryFind x.Date rf with 
        | None -> failwith $"why isn't there a rf for {x.Date}"
        | Some rf -> { x with Return = x.Return - rf })

let getStockData portfolio =
    Array.concat [| standardInvestmentsExcess; portfolio |]
    |> Array.groupBy(fun x -> x.Symbol)
    |> Map

let getSymbols stockData = 
    stockData 
    |> Map.toArray // convert to array of (symbol, observations for that symbol) 
    |> Array.map fst // take just the symbol
    |> Array.sort // sort them

// LONG PORTFOLIO
let stockDataLong = getStockData long
let symbolsLong = getSymbols stockDataLong

// LONG SHORT PORTFOLIO
let stockDataLS = getStockData longShort
let symbolsLS = getSymbols stockDataLS

(**
Create a function that calculates covariances
for two securities.
*)
// Get a ticker symbol x and ticker symbol y and get the stockData for them and get the Covariance
let getCov x y stockData =
    // This code is making sure we have data from overlapping periods
    let innerJoin xId yId =
        let xRet = Map.find xId stockData
        let yRet = Map.find yId stockData |> Array.map(fun x -> x.Date, x) |> Map
        xRet
        |> Array.choose(fun x ->
            match Map.tryFind x.Date yRet with
            | None -> None
            | Some y -> Some (x.Return, y.Return))
    let x, y = innerJoin x y |> Array.unzip
    Seq.cov x y

let getCovariances symbols stockData=
    symbols
    |> Array.map(fun x ->
        symbols
        |> Array.map(fun y -> getCov x y stockData))
    // We get a covariance matrix
    |> matrix

// The DIAGONAL is the variance 
// COVARIANCES
let covariancesLong = getCovariances symbolsLong stockDataLong
let covariancesLS = getCovariances symbolsLS stockDataLS

// Get Portfolio Mean Returns
let getMeans (stockData:Map<string,StockData array>) = 
    stockData
    |> Map.toArray
    |> Array.map(fun (sym, xs) ->
        sym,
        xs |> Array.averageBy(fun x -> x.Return))
    |> Array.sortBy fst
    |> Array.map snd
    |> vector

// MEANS
let meanLong = getMeans stockDataLong
let meanLS = getMeans stockDataLS    

(**
This solution method for finding the tangency portfolio
comes from Hilliar, Grinblatt and Titman 2nd
European Edition, Example 5.3. 
*)
type statistics = {Portfolio: string; AnnualizedSharpeRatio: float; StandardDev: float; Mean: float; Variance: float; weights: Vector<float>}
let tangency covariances means (name:string) = 
    // solve A * x = b for x
    let w' = Algebra.LinearAlgebra.SolveLinearSystem covariances means
    let w = w' |> Vector.map(fun x -> x /  Vector.sum w')
    // Portfolio variance
    let portVariance = w.Transpose * covariances * w
    // Portfolio standard deviation
    let portStDev = sqrt(portVariance)
    // Portfolio mean 
    let portMean = w.Transpose * means
    // Annualized Sharpe ratio
    let annualizedSharpeR = sqrt(12.0)*(portMean/portStDev)
    { Portfolio = name; 
      AnnualizedSharpeRatio = annualizedSharpeR;
      StandardDev = portStDev; 
      Mean = portMean; 
      Variance = portVariance;
      weights = w}

// Get Statistics
let tangencyLong = tangency covariancesLong meanLong "Long Portfolio"
let tangencyLS = tangency covariancesLS meanLS "Long-Short Portfolio"

(**
## Comparing mean-variance efficient to 60/40.

Now let's form the mean-variance efficient portfolios
based on the above optimal weights and compare them to
a 60/40 portfolio over our sample. A 60% stock and 40%
bond portfolio is a common investment portfolio.
*)
(**
Get the symbol data grouped by date.
*)

let getStockDataByDate stockData =
    stockData
    |> Map.toArray // Convert to array of (symbol, StockData)
    |> Array.map snd // grab only the stockData from (symbol, StockData)
    |> Array.collect id // combine all different StockData symbols into one array.
    |> Array.groupBy(fun x -> x.Date) // group all symbols on the same date together.
    |> Array.sortBy fst // sort by the grouping variable, which here is Date.

(**
Now if we look we do not have all the symbols on all the dates.
This is because our ETFs (VTI, BND) did not start trading until later in our sample
and our strategy ended at the end of the year, while we have stock quotes
for VTI and BND trading recently.

Get start and end for all 3 assets
*)

let getAllAssetsStart (stockDataByDate:(DateTime * StockData array) array) (stockData:Map<'a,StockData array>) =
    stockDataByDate
    // find the first array element where there are as many stocks as you have symbols
    |> Array.find(fun (month, stocks) -> stocks.Length = (getSymbols stockData).Length)
    |> fst // convert (month, stocks) to month
    
let getAllAssetsEnd (stockDataByDate:(DateTime * StockData array) array) (stockData:Map<'a,StockData array>) =
    stockDataByDate
    // find the last array element where there are as many stocks as you have symbols
    |> Array.findBack(fun (month, stocks) -> stocks.Length = (getSymbols stockData).Length)
    |> fst // convert (month, stocks) to month
(**
Ok, let's filter the data between those dates.
*)

let getStockDataByDateComplete (stockDataByDate:(DateTime * StockData array) array) allAssetsStart allAssetsEnd= 
    stockDataByDate
    |> Array.filter(fun (date, stocks) -> 
        date >= allAssetsStart &&
        date <= allAssetsEnd)

(**
Now let's make the mve and 60/40 port a function that takes weights and monthData as input
*)

let portfolioMonthReturn weights monthData =
    weights
    |> Map.toArray
    |> Array.map(fun (symbol, weight) ->
        let symbolData = 
            // we're going to be more safe and use tryFind here so
            // that our function is more reusable
            match monthData |> Array.tryFind(fun x -> x.Symbol = symbol) with
            | None -> failwith $"You tried to find {symbol} in the data but it was not there"
            | Some data -> data
        symbolData.Return*weight)
    |> Array.sum    
    
(**
Weights are sorted by `symbols`
*)
let weights tangencyPort (stockData:Map<'a,StockData array>)=
    Seq.zip (getSymbols stockData) tangencyPort.weights
    |> Map.ofSeq
 
// 60/40 weights
let weights6040 = Map [("VTI",0.6);("BND",0.4)]

let getPortfolio stockData weights (name:string) = 
    let stockDataByDate = getStockDataByDate stockData
    let allAssetsStart = getAllAssetsStart stockDataByDate stockData
    let allAssetsEnd = getAllAssetsEnd stockDataByDate stockData
    let stockDataByDateComplete = getStockDataByDateComplete stockDataByDate allAssetsStart allAssetsEnd
    stockDataByDateComplete
    |> Array.map(fun (date, data) -> 
        { Symbol = name
          Date = date
          Return = portfolioMonthReturn weights data })

let portMveLong = getPortfolio stockDataLong (weights tangencyLong stockDataLong) "MVE-Long"
let portMveLS = getPortfolio stockDataLS (weights tangencyLS stockDataLS) "MVE-Long-Short"
let port6040 = getPortfolio stockDataLong (weights6040) "60/40"

// STATS

type stats6040 = {PortfolioId : string; AnnualizedReturn: float; AnnualizedSharpeRatio : float; AnnualizedStDev : float}
let statistics portfolio (name:string) =
    let retr = portfolio |> Array.averageBy(fun x -> x.Return)
    let annualizedRetr = 12.0*retr
    let stdev = portfolio |> stDevBy(fun x -> x.Return)
    let annualizedStdev = sqrt(12.0) * stdev
    let annualizedSharpeRatio = annualizedRetr/annualizedStdev
    {PortfolioId = name; 
     AnnualizedReturn = annualizedRetr;
     AnnualizedSharpeRatio = annualizedSharpeRatio;
     AnnualizedStDev = annualizedStdev }
    
statistics portMveLong "MVE-Long"
statistics portMveLS "MVE-Long-Short"
statistics port6040 "60/40"

(**
PLOT
*)
#r "nuget: Plotly.NET, 2.0.0-beta9"
open Plotly.NET
(**
A function to accumulate returns.
*)
let cumulateReturns xs =
    let mapFolder prevRet x =
        let newReturn = prevRet * (1.0+x.Return)
        { x with Return = newReturn}, newReturn
    (1.0, xs) 
    ||> Array.mapFold mapFolder
    |> fst    
(**
Cumulative returns.
*)
let portMveLongCumulative = portMveLong |> cumulateReturns
let portMveLSCumulative = portMveLS |> cumulateReturns
let port6040Cumulative = port6040 |> cumulateReturns


let chartMveLong = 
    portMveLongCumulative
    |> Array.map(fun x -> x.Date, x.Return)
    |> Chart.Line
    |> Chart.withTraceName "MVE-Long"

let chartMveLS = 
    portMveLSCumulative
    |> Array.map(fun x -> x.Date, x.Return)
    |> Chart.Line
    |> Chart.withTraceName "MVE-Long-Short"

let chart6040 = 
    port6040Cumulative
    |> Array.map(fun x -> x.Date, x.Return)
    |> Chart.Line
    |> Chart.withTraceName "60/40"

let chartCombined =
    [| chartMveLong; chartMveLS; chart6040 |]
    |> Chart.Combine
chartCombined |> Chart.Show

(**
Those are partly going to differ because they have different volatilities.
It we want to have a sense for which is better per unit of volatility,
then it can make sense to normalize volatilities.

Plot Cumulative returns of the normalized vol returns.
*)
let normalize10pctVol xs =
    let vol = xs |> Array.map(fun x -> x.Return) |> Seq.stDev
    let annualizedVol = vol * sqrt(12.0)
    xs 
    |> Array.map(fun x -> { x with Return = x.Return * (0.1/annualizedVol)})

let portMveLongCumulativeNormlizedVol = 
    portMveLong
    |> normalize10pctVol
    |> cumulateReturns

let portMveLSCumulativeNormlizedVol = 
    portMveLS
    |> normalize10pctVol
    |> cumulateReturns

let port6040CumulativeNormlizedVol = 
    port6040
    |> normalize10pctVol 
    |> cumulateReturns

// Charts
let chartMveLongNormlizedVol = 
    portMveLongCumulativeNormlizedVol
    |> Array.map(fun x -> x.Date, x.Return)
    |> Chart.Line
    |> Chart.withTraceName "MVE-Long Normalized"

let chartMveLSNormlizedVol = 
    portMveLSCumulativeNormlizedVol
    |> Array.map(fun x -> x.Date, x.Return)
    |> Chart.Line
    |> Chart.withTraceName "MVE-Long-Short Normalized"

let chart6040NormlizedVol = 
    port6040CumulativeNormlizedVol
    |> Array.map(fun x -> x.Date, x.Return)
    |> Chart.Line
    |> Chart.withTraceName "60/40 Normalized"

let chartCombinedNormlizedVol =
    [| chartMveLongNormlizedVol; chartMveLSNormlizedVol; chart6040NormlizedVol |]
    |> Chart.Combine
chartCombinedNormlizedVol |> Chart.Show

let stackedChart = 
    [ chartCombined
      chartCombinedNormlizedVol ]
    |> Chart.Stack(2,0.05)
    |> Chart.withSize(1200.,450.)
    |> Chart.withTitle "Growth of 1 Euro"
    |> Chart.Show
