// Last updated 2021-04-29 09:00
#time "on"

#r "nuget: XPlot.Plotly"
#r "nuget: FSharp.Data"
#r "nuget: MathNet.Numerics"
#r "nuget: FSharp.Collections.ParallelSeq"
#r "nuget: NodaTime"

#load "Portfolio2.fsx"

open System
open FSharp.Collections.ParallelSeq
open System.IO
open System.IO.Compression
open MathNet.Numerics
open MathNet.Numerics.Statistics
open Newtonsoft.Json
open FSharp.Data
open XPlot.Plotly
open NodaTime
open NodaTime.Calendars

open Portfolio2

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

let samplePeriod x = 
    x >= YearMonth(2010, 1) &&
    x <= YearMonth(2020, 2)

(**
# Price momentum
Price momentum is one of the most common quant signals. It is (fairly)
straight forward to calculate, and you only need returns to do it,
so it is a good starting point and reference 'strategy'.
*)

// Now create a type that represents the file.
// This figures out what the columns of the file are.
//  - Sample is the path to our file. The "../" means we're
//    doing relative paths, so we need to specify the 
//  - ResolutionFolder to indicate what folder the relative paths
//    are relative to.
type MsfCsv = CsvProvider<Sample="../data-cache/msf-momentum2.csv",
                          ResolutionFolder = __SOURCE_DIRECTORY__>

// assign the content of the file to a value
let msfCsv = MsfCsv.GetSample()  // We can do this because we defined the sample as the full file 
let data = MsfCsv.Load(__SOURCE_DIRECTORY__ + "/../data-cache/msf-momentum2.csv")
data.Take 5
// look at the file attributes
msfCsv
// look at the headers
msfCsv.Headers
// look at the first few rows
msfCsv.Rows |> Seq.truncate 3  //truncate vs take -> if we only have 3 things and try to take 5, it gives an error. truncate takes (At most) the number you specify

(**
## Signal construction
We want to create a momentum signal and see how it relates to future returns.
The signal is some measure of past returns. A common measure is the past year return,
skipping the most recent month. We skip the most recent month because stocks tend
to reverse following very recent returns (known as "reversals"). 
The reversal is very likely a liquidity effect and it is less important this century.
So returns are positively correlated with returns from 12 months to 1 months ago, 
but negatively correlated with returns last month. This is illustrated very nicely
in Jegadeesh (1990).

Imagine a regression on month 12, 11, 10...1 until (today) -> last month month has a negative effect 

If we're forming a portfolio at the end of month $t-1$ to hold in month $t$, 
then we're saying returns in month $t$ are positively correlated
with returns in months $t-12$ through month $t-2$.
For example, if we want to hold a momenum portfolio in January 2021, 
then we will form it on December 31, 2020.
We will want to go long stocks that had high returns from the beginning 
of January 2020 to the end of November 2020.

Let's create a record to hold some info about past returns for a stock.
We will use this as a trading signal.
*)

type PriorReturnOb = 
    { SecurityId : SecurityId
      FormationMonth : YearMonth 
      Retm12m2 : float
      N : int }

(**
Note the `YearMonth` type for portfolio formation month.
This type is from the library [NodaTime](https://nodatime.org/). 

Why are we using it? Date math is hard and easy to mess up.

We're dealing with monthly data.
If we use `DateTime`, then we have to give the month a 
day value. We could always use the first day of the month,
but then month return goes all the way to the end of the month.
And we might forget that information. 

If we use the last day of the month,
then what happens when we add months. For example,
we have to start doing things like.
*)

let endOfFebruary = DateTime(2020,2,28)
let endOfFebruaryPlus1Month = endOfFebruary.AddMonths(1)
let endOfMarch = DateTime(endOfFebruary.Year,endOfFebruary.Month,1).AddMonths(2).AddDays(-1.0)
endOfFebruaryPlus1Month = endOfMarch // evaluates to false

(**
That's kind of ugly. 

We also have to worry about things like what happens
if we add a month but it overlapped with daylight savings time?
What about timezones? If we're never dealing with times,
it's nice to ignore all these things.

This is nicer way of doing it using nodatime's `YearMonth`:
*)

let february = YearMonth(2020,2) 
let februaryPlus1Month = february.PlusMonths(1)
let march = YearMonth(2020,3)
februaryPlus1Month = march // true

(**
Let's focus on a single stock.
*)
// let exampleRow = 
//     msfCsv.Rows
//     |> Seq.filter(fun x -> x.Ticker = "AMZN")
//     |> Seq.head
// let key = (Permno exampleRow.Permno, YearMonth(exampleRow.Year, exampleRow.Month))
// fst key    

let amznReturns = 
    // we're filtering and then storing as a map.
    // if we used a sequence instead of a map/arra/list, then
    // every time we used amznReturns, the sequence
    // would be recreated by filtering msfCsv.Rows.
    // That's one difference between (lazy) seq and
    // (eager) array/list/map.
    msfCsv.Rows
    |> Seq.filter(fun x -> x.Ticker = "AMZN")
    |> Seq.map(fun x ->
        let ym = YearMonth(x.Month.Year,x.Month.Month) 
        let key = Permno x.Permno, ym
        key, x)
    |> Map.ofSeq
  
// Using the Map
Map.tryFind (Permno 84788, YearMonth(2017, 12)) amznReturns

let getPastYearObs2 returns (security:SecurityId, formationMonth:YearMonth) =
    [|-11 .. -1|]
    |> Array.choose(fun i ->
        let returnMonth = formationMonth.PlusMonths(i)
        Map.tryFind (security, returnMonth) returns)

// Get returns from -11 months to -1 month
let getPastYearObs 
    (returns:Map<(SecurityId * YearMonth),MsfCsv.Row>)
    (security: SecurityId, formationMonth: YearMonth) =
        [| -11 .. -1 |]
        |> Array.choose(fun i -> 
            let returnMonth = formationMonth.PlusMonths(i)
            Map.tryFind (security, returnMonth) returns)    

// check Permno 84788 is Amzn
getPastYearObs amznReturns (Permno 84788, YearMonth(2019,12))  
getPastYearObs amznReturns (Permno -400, YearMonth(2019,1))  // empty array because we are using Array.choose and it chooses only stocks that exist

// making cumulative returns 
let cumulativeReturn rets =
    // using Seq so that it will work with any collection
    let grossReturn = (1.0, rets) ||> Seq.fold(fun acc ret -> acc * (1.0 + ret))
    grossReturn - 1.0

// check
cumulativeReturn []
cumulativeReturn [1.0;-0.5]
cumulativeReturn [1.0;-0.75]


(**
We're now ready to create our Momentum signal function.
*)

// If you don't want to write the typessecurity, month all the time.
// 

// past year cumulative return for AMZ
let priorObs = getPastYearObs amznReturns (Permno 84788, (YearMonth(2019, 1)))
let priorRets = priorObs |> Array.choose(fun x -> x.Ret)
cumulativeReturn priorRets


let getMomentumSignal returns (security, formationMonth) =
    let priorObs = getPastYearObs  returns (security, formationMonth)
    let priorRets = priorObs |> Array.choose(fun x -> x.Ret)
    // We should probably return None if there are no observations.
    // If they are all missing, Array.choose will return an empty
    // array. See:
    // ([| None; None |]: int option []) |> Array.choose id
    //
    // So we'll check for an empty array and return None in that case.
    if Array.isEmpty priorRets then
        None 
    else
        Some { SecurityId = security 
               FormationMonth = formationMonth
               Retm12m2 = cumulativeReturn priorRets
               N = priorRets.Length }

// Check
getMomentumSignal amznReturns (Permno 84788, YearMonth(2019,1)) 
getMomentumSignal amznReturns (Permno -400, YearMonth(2019,1))  

(**
One thing you may notice is that our momentum signal function gets everything from
it's inputs. That means that if we give it different intputs then
we could get momentum signals for other stocks. 

For example we can create a map collection like we had for amzn, but for all stocks.
*)

let msfByPermnoMonth =
    msfCsv.Rows
    |> Seq.map(fun x ->
        let ym = YearMonth(x.Month.Year,x.Month.Month) 
        let key = Permno x.Permno, ym
        key, x)
    |> Map.ofSeq

// finding some permnos for notable tickers

// don't use tickers. companies change tickers, so you might look up the wrong company
// That's why I'm picking some tickers that I know haven't changed, but my function
// is using PERMNO.

let notableTicks =
    ["MSFT";"AAPL"]
    |> Seq.map(fun tick -> 
        msfCsv.Rows
        // hover over 'find' in 'Seq.find' if you don't remember what it does. 
        |> Seq.find(fun row -> row.Ticker = tick)
        |> fun row -> row.Ticker, row.Permno)
    |> Map.ofSeq

let msftTestIndex = (Permno notableTicks.["MSFT"], YearMonth(2019,1))
let aaplTestIndex = (Permno notableTicks.["AAPL"], YearMonth(2019,1))  

getMomentumSignal msfByPermnoMonth msftTestIndex 
getMomentumSignal msfByPermnoMonth aaplTestIndex  

(*
and we can use [partial function application](https://fsharpforfunandprofit.com/posts/partial-application/)
to "bake in" the msfByPermnoMonth parameter so that we don't keep having to pass it around.
*)

let getMomentumSignalAny = getMomentumSignal msfByPermnoMonth

getMomentumSignalAny msftTestIndex 
getMomentumSignalAny aaplTestIndex  

(**
## Defining the investment universe

Let's say we have a portfolio formation month. Can we look up securities available to invest in?

*)

let securitiesByFormationMonth =
    msfCsv.Rows
    |> Seq.groupBy(fun x -> YearMonth(x.Month.Year, x.Month.Month))
    |> Seq.map(fun (ym, xs) -> 
        ym, 
        xs 
        |> Seq.map(fun x -> Permno x.Permno) 
        |> Seq.toArray)
    |> Map.ofSeq

// put in your month, and you get all the available securities
Map.tryFind (YearMonth(2020, 1)) securitiesByFormationMonth

let getInvestmentUniverse formationMonth =
    match Map.tryFind formationMonth securitiesByFormationMonth with
    | Some securities -> 
        { FormationMonth = formationMonth 
          Securities = securities }
    | None -> failwith $"{formationMonth} is not in the date range"      

getInvestmentUniverse (YearMonth(2011,10))
// getInvestmentUniverse (YearMonth(1990,10))

YearMonth(2015,3)
|> getInvestmentUniverse

(**
You might also want to filter the investment universe by some criteria.
*)


let isCommonStock securityFormationMonth =
    match Map.tryFind securityFormationMonth msfByPermnoMonth with
    | None -> false
    | Some x -> List.contains x.Shrcd [10; 11]

let onNyseNasdaqAmex securityFormationMonth =
    match Map.tryFind securityFormationMonth msfByPermnoMonth with
    | None -> false
    | Some x -> List.contains x.Exchcd [ 1; 2; 3]

let hasPrice13mAgo (security, formationMonth:YearMonth) =
    //13m before the holding month, 12m before the formation month
    match Map.tryFind (security, formationMonth.PlusMonths(-12)) msfByPermnoMonth with
    | None -> false
    | Some m13 -> m13.Prc.IsSome

let hasReturn2mAgo (security, formationMonth:YearMonth) =
    //2m before the holding month, 1m before the formation month
    match Map.tryFind (security, formationMonth.PlusMonths(-1)) msfByPermnoMonth with
    | None -> false
    | Some m2 -> m2.Ret.IsSome

let hasMe1mAgo (security, formationMonth) =
    //1m before the holding month, so the formation month
    match Map.tryFind (security, formationMonth) msfByPermnoMonth with
    | None -> false
    | Some m1 -> m1.Prc.IsSome && m1.Shrout.IsSome

let has8ReturnsPastYear securityFormationMonth =
    match getMomentumSignalAny securityFormationMonth with 
    | None -> false 
    | Some x -> x.N >= 8

let danielMoskowitzRestrictions securityFormationMonth =
    isCommonStock securityFormationMonth &&
    onNyseNasdaqAmex securityFormationMonth &&
    hasPrice13mAgo securityFormationMonth &&
    hasReturn2mAgo securityFormationMonth &&
    hasMe1mAgo securityFormationMonth &&
    has8ReturnsPastYear securityFormationMonth 

let restrictUniverse (investmentUniverse: InvestmentUniverse) =
    let filtered =
        investmentUniverse.Securities
        |> Array.filter(fun security -> 
            danielMoskowitzRestrictions (security, investmentUniverse.FormationMonth))
    { FormationMonth = investmentUniverse.FormationMonth
      Securities = filtered }        

(**
Now we can see where we are.
*)

YearMonth(2011,10)
|> getInvestmentUniverse
|> restrictUniverse

let investmentUniverse =
    YearMonth(2011,10)
    |> getInvestmentUniverse
let restrictedInvestmentUniverse =
    investmentUniverse |> restrictUniverse

// See if we're excluding some securities.
investmentUniverse.Securities.Length
restrictedInvestmentUniverse.Securities.Length

(**
We can get our Momentum signals
*)

let getMomentumSignals (investmentUniverse: InvestmentUniverse) =
    { FormationMonth = investmentUniverse.FormationMonth 
      Signals = 
        investmentUniverse.Securities
        |> Array.choose(fun security -> 
            match getMomentumSignalAny (security, investmentUniverse.FormationMonth ) with
            | None -> None 
            | Some x -> 
                let signal =
                    { SecurityId = security 
                      Signal = x.Retm12m2 }
                Some signal) }

restrictedInvestmentUniverse
|> getMomentumSignals
