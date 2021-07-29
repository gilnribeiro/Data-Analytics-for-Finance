(**
# Federal Reserve Economic Data (FRED)

[FRED](https://fred.stlouisfed.org/) is a good source for US economic data. We will use it as a source for long time series of US bond returns.

*)

#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET, 2.0.0-beta5"
open System
open FSharp.Data
open FSharp.Stats
open Plotly.NET

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))


(**
Say we want [10 year constant maturity interest rates](https://fred.stlouisfed.org/series/GS10).
*)

let gs10 = CsvProvider<"https://fred.stlouisfed.org/graph/fredgraph.csv?id=GS10">.GetSample()

// Inspect what gs10 looks like
gs10

(***do-not-eval***)
gs10.Rows
|> Seq.map(fun x -> x.DATE, x.GS10)
|> Chart.Line
|> Chart.withTitle "10-year rate"
|> Chart.Show

(*** include-it ***)

(** or [20 year rates](https://fred.stlouisfed.org/series/GS20), change the id at the end. *)
let gs20 = CsvProvider<"https://fred.stlouisfed.org/graph/fredgraph.csv?id=GS20">.GetSample()
gs20

(** Self-check: Can you plot the 20-year constant maturity interest rate? *)

(*** include-it ***)

(** 

Can we create our own API to FRED? If you notice, all that's changing is the name of
the series. We can generalize this and also account for missing values 
(I know from experience that FRED returns missing data as "." in this api). To do this,
we create a proper FRED type provider, and override the series column header to be 
called "Value" so that it is more generic. We also tell the CsvProvider to represent
the numeric value as a float (which represents missing values with `nan`).

*)

type Fred = CsvProvider<"https://fred.stlouisfed.org/graph/fredgraph.csv?id=GS10",
                         Schema="Date,Value (float)",
                         MissingValues=".">
let fredUrl series = $"https://fred.stlouisfed.org/graph/fredgraph.csv?id={series}"

// Now we can obtain series like so:
let usdPerEuro = Fred.Load(fredUrl "DEXUSEU")
usdPerEuro.Rows |> Seq.rev |> Seq.take 5

(*** include-it ***)

let chart1y10y =
    ["GS1", "1-Year"
     "GS10", "10-Year"]
    |> Seq.map(fun (series, name) -> 
        Fred.Load(fredUrl series).Rows
        |> Seq.map(fun row -> row.DATE, row.Value)
        |> Chart.Line 
        |> Chart.withTraceName name)
    |> Chart.Combine
    |> Chart.withTitle "Treasury Constant Maturity Rates"
    |> Chart.withY_AxisStyle(title = "Interest Rate (%)")

(*** do-not-eval ***)

chart1y10y |> Chart.Show

(***hide***)
chart1y10y |> GenericChart.toChartHTML
(***include-it-raw***)


(**
A good short-term risk-free rate is US t-bills.
*)
// Take a look at a few recent days.
let threeMonthBills = Fred.Load(fredUrl "DTB3")
threeMonthBills.Rows
|> Seq.sortByDescending(fun x -> x.DATE)
|> Seq.take 10
|> Seq.toList

(**
### Map Collections
We want to be able to look up a risk-free rate on a given day. If we're doing lookups, then a good data structure for that is a map. Maps consist of key and value pairs. If you look up the key, you get the value.
33
If you need to do lookups on a key, maps are much more efficient than trying to do the same thing
with a list or array. Some more info [here](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-mapmodule.html).
*)

let exampleMap = Map [("a", "hey"); ("b","ho")]
let exampleMap2 = [(4,"apple"); (10,"pizza")] |> Map
exampleMap.["a"]
Map.find "a" exampleMap
exampleMap2 |> Map.find 10

// Comparing performance of array vs. Map lookups.

(***do-not-eval***)
#time "on"
let isOdd x = if x % 2 = 0 then false else true
let arr = [| for i = 1 to 100_000 do (i, isOdd i)|]
let arrMap = arr |> Map

arr |> Array.find (fun (a,b) -> a = 100)
arrMap |> Map.find 101

// Compare performance to find something at the beginning of an array.
for i = 1 to 100 do 
    arr |> Array.find(fun (a,b) -> a = 1_000) |> ignore

for i = 1 to 100 do
    arrMap |> Map.find 1_000 |> ignore

// Compare performance to find something that is towards the end of the array.
for i = 1 to 100 do 
    arr |> Array.find(fun (a,b) -> a = 99_000) |> ignore

for i = 1 to 100 do
    arrMap |> Map.find 99_000 |> ignore


(**
### Option type
Somtimes something exists or doesn't exist. This can be useful to model explicitly. [FSharp For Fun And Profit](https://fsharpforfunandprofit.com/posts/the-option-type/) has a nice discussion of option types and how to use them. You can also find information on the [tour of F#](https://docs.microsoft.com/en-us/dotnet/fsharp/tour#optional-types) and the [F# language reference](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-core-optionmodule.html).

The main purpose is to model situations where you could have "something" or "nothing" explicitly.

For example, our `exampleMap` does not have anything associated with key `"c"`. If we try to access `exampleMap.["c"]` we will get an exception. The preferred way to do this is to try to get the key, and then if there is no value for that key return nothing. This is what the option time is useful. Options are either `Some x` or `None`, where `x` is the data that you want. This is what the "..try" functions are about.


*)

Map.tryFind "a" exampleMap
Map.tryFind "c" exampleMap

// Other option examples
let xx = Some 4.0
let yy = None

xx |> Option.map(fun x -> x + 1.0)
yy |> Option.map (fun x -> x + 1.0)

let xxyy = [xx; yy; xx; yy; xx |> Option.map(fun x -> x + 100.0)] 

xxyy

let divideBy2 x = x / 2.0
xxyy 
|> List.map(fun x -> 
    x |> Option.map divideBy2
)

xxyy 
|> List.choose(fun x -> 
    x |> Option.map divideBy2
)

(**
### Estimating a bond return from changing interest rates
Now, that we have a data structure that we can use for interest rate look-ups,
we want to populate it with returns on risk-free assets.

The T-bill rates that we looked at just now were interest rates quoted on
a particular flavor of annual basis. We don't want the annual interest rate. 
We want the return on the risk-free asset over a particular period. 
T-bills are zero-coupon bonds, so we can calculate the return based on the changing price.

To do so, we need to use the interest rate to obtain the price.
*)

let priceFromDiscountBasisYield days y =
    // https://www.newyorkfed.org/aboutthefed/fedpoint/fed07.html
    Math.Round(10_000.0 - (y*100.0 * days)/360.0,2)
if (priceFromDiscountBasisYield 169.0 5.08) <> 9761.52 then
    failwith "Didn't pass FED example"

// Now calculate T-bill returns and populate our Map.
let dailyRiskFreeRate =
    threeMonthBills.Rows
    |> Seq.map(fun row -> row.DATE, row.Value)
    |> Seq.sortBy fst
    |> Seq.pairwise 
    |> Seq.map(fun ((priorDt, priorYld), (dt, yld)) -> 
        // DTB3 is secondary market 3-month yield.
        // So technically *not* constant maturity, 
        // but we'll assume constant and flat term structure. 
        // Flat term structure means yesterday's bonds
        // trade at the same yield as today
        let timeDiff = (dt - priorDt).Days |> float
        let p1 = priceFromDiscountBasisYield 90.0 priorYld
        let p2 = priceFromDiscountBasisYield (90.0 - timeDiff) yld
        dt, (p2/p1 - 1.0))
    |> Map

// Finding a date on a given day.
Map.tryFind (DateTime(2020,12,31)) dailyRiskFreeRate

// But, we can't find risk-free rates for future days
Map.tryFind (DateTime.Now.AddDays(1.0)) dailyRiskFreeRate
// Or weekends
Map.tryFind (DateTime(2020,12,5)) dailyRiskFreeRate

(**
### More Pattern Matching: Match expressions.
Some additional examples with `Match` statements. We're going to start using these.
There is a very good discussion with examples on [FSharp For Fun and Profit](https://fsharpforfunandprofit.com/posts/match-expression/).

The idea is that you give the match expression a value to match, the things it can match to, and then what you want to return for each of those matches.
*)

let f1 x =
    match x with // x is the thing that I am matching
    | 1.0 -> 1.0 // when x matches 1.0 then return 1.0
    | 2.0 -> 1.0 // when x matches with 2.0 then return 1.0
    | 3.0 -> 7.0 + 7.0 // when x matches with 3.0 return 7.0+7.0
    | y -> y ** 2.0 // everything else matches an arbitrary y, and let's return y**2.0

[ 1.0 .. 10.0] |> List.map f1

(**

### Recursive functions.
Ok, now what do we do if we look for an interest rate on a weekend? If we check on a day without a rate, we could check the day before. Here's how to do this using a recursive function. We define recursive functions by putting `rec` in front of them.

*)

let rec tempGetRate (date:DateTime) =
    // First, try to find the riskfree rate on our date
    match Map.tryFind date.Date dailyRiskFreeRate with
    // If we find it, return the rate
    | Some rate -> date, rate
    // If we do not find anything, try again on the prior day
    | None -> tempGetRate (date.AddDays(-1.0))

// Check a weekend
tempGetRate (DateTime(2020,12,5))
tempGetRate (DateTime(2020,12,6))


(**

Any gotchas? Well, if the date we check is earlier than our first risk-free rate, then we will keep checking forever and never find anything. Probably, we want to stop checking after some point.

*)

/// Tries to find the latest risk-free rate on or before a given date.
let getDailyRiskFree (date:DateTime) =
    let rec loop (innerDate:DateTime) =
        match Map.tryFind innerDate.Date dailyRiskFreeRate with
        | Some rate -> rate
        | None -> 
            // Here we see the check.
            if (date - innerDate).Days > 30 then
                // failwith stops the whole program with an exception.
                // I'm ok with that here because if this gets triggered
                // there's some big error in the code somewhere and I
                // want to crash the script. In a compiled program,
                // you would want to handle this more gracefully. 
                failwith "Could not find one within 30 days"
            else    
                loop (innerDate.AddDays(-1.0))
    loop date
    
getDailyRiskFree (DateTime(2020,12,6))