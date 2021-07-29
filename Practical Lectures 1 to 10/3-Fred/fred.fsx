(**
# Federal Reserve Economic Data (FRED)

[FRED](https://fred.stlouisfed.org/) is a good source for US economic data. We will use it as a source for long time series of US bond returns.

*)

#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET, 2.0.0-beta5"
open System
open System.IO
open FSharp.Data
open FSharp.Stats
open Plotly.NET

// #load "../Common.fsx"
// open Common

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

let cacheDirectory = "../data-cache/"

(**
Say we want [10 year constant maturity interest rates](https://fred.stlouisfed.org/series/GS10).
*)

// This downloads the csv file
// if is a well formated csv this is enough
let gs10 = CsvProvider<"https://fred.stlouisfed.org/graph/fredgraph.csv?id=GS10">.GetSample()

// Inspect what gs10 looks like
gs10.Rows
|> Seq.map (fun row -> row.DATE, row.GS10)
|> Seq.take 5

////// PRACTICE
gs10.Rows
|> Seq.groupBy (fun row -> row.DATE.Year, row.DATE.Month)
|> Seq.map (fun (year, ret) -> 
    year, ret |> Seq.averageBy (fun x -> x.GS10))

gs10.Rows
|> Seq.groupBy (fun row -> row.DATE.Year, row.DATE.Month)
|> Seq.map (fun (year, rowsInTheYear) -> 
    year, rowsInTheYear |> Seq.maxBy (fun row -> row.GS10))
(*** include-it ***)

(** or [20 year rates](https://fred.stlouisfed.org/series/GS20), change the id at the end. *)
let gs20 = CsvProvider<"https://fred.stlouisfed.org/graph/fredgraph.csv?id=GS20">.GetSample()
gs20

//// PRACTICE
// Group 20 year bond by yaer and find the month with the minimum interest rate
gs20.Rows
|> Seq.groupBy (fun row -> row.DATE.Year)
|> Seq.map (fun (year, rowsInTheYear) -> 
    year, rowsInTheYear |> Seq.minBy (fun row -> row.GS20))


(*** include-it ***)

(** 

Can we create our own API to FRED? If you notice, all that's changing is the name of
the series. We can generalize this and also account for missing values 
(I know from experience that FRED returns missing data as "." in this api). To do this,
we create a proper FRED type provider, and override the series column header to be 
called "Value" so that it is more generic.

*)

// Schema parameter overrides the column headers
// Missing values as "." -> Option Type

// Option type
type OptionRecord = { Value : string Option}
let x = {Value = Some "1.0"}
let y = { Value = None}
let parseOptionStringToFloat (x: string Option) = 
    x |> Option.map(fun x -> float x)

parseOptionStringToFloat x.Value
parseOptionStringToFloat y.Value

// This is a way to code in  the types the value can be missing and  i want to be sure I always handle the missing case
// This integrates missing value handling in the type

type Fred = CsvProvider<"https://fred.stlouisfed.org/graph/fredgraph.csv?id=GS10",
                         Schema="Date,Value (decimal option)",
                         MissingValues=".">
// Create a functio call fredUrl giving it a series
let fredUrl series = $"https://fred.stlouisfed.org/graph/fredgraph.csv?id={series}"
fredUrl "GS20"

// Now we can obtain series like so:
// This creates a URL with the exchange rate we specify
let usdPerEuro = Fred.Load(fredUrl "DEXUSEU")
usdPerEuro.Rows |> Seq.rev |> Seq.take 5

(*** include-it ***)

// practice
[Some 1.0; Some 2.0; None; Some 4.0]
// |> List.filter (fun x -> x.IsSome)
|> List.choose (fun x -> x)


// Im going to create a list of tuples (so, the fred code of the list  i want to download)
let chart1y10y =
    ["GS1", "1-Year"
     "GS10", "10-Year"]
    |> Seq.map(fun (series, name) -> 
        Fred.Load(fredUrl series).Rows
        |> Seq.choose(fun row -> 
            // This is to check if there are missing values and handling those
            match row.Value with
            | None -> None
            | Some rate -> Some (row.DATE, rate))
        // Option.map (fun x -> row.DATE, x) row.Value)
        // CHART PART
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

(** Self-check: Can you plot the USD/Euro exchange rate? *)

let chart2 =
    ["DEXUSEU", "US/EUR exchange Rate"]
    |> Seq.map(fun (series, name) -> 
        Fred.Load(fredUrl series).Rows
        |> Seq.choose(fun row -> 
            // This is to check if there are missing values and handling those
            match row.Value with
            | None -> None
            | Some rate -> Some (row.DATE, rate))
        // Option.map (fun x -> row.DATE, x) row.Value)
        // CHART PART
        |> Chart.Line 
        |> Chart.withTraceName name)
    |> Chart.Combine
    |> Chart.withTitle "US / EURO Exchange Rate"
    |> Chart.withY_AxisStyle(title = "Exchange Rate")
    |> Chart.withLegend true
    |> Chart.withSize (5.0, 5.0)

chart2 |> Chart.Show

/// Teachers example
let euro = Fred.Load(fredUrl "DEXUEU")
euro.Rows
|> Seq.choose (fun row -> 
    match row.Value with 
    | None -> None
    | Some exchangeRate -> Some (row.DATE, exchangeRate)
    )
|> Chart.Line
|> Chart.Show


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
*)

// Basically an immutable dictionary
let exampleMap = Map [("a", "hey"); ("b","ho")]
exampleMap.["a"]


let exampleMap2 = [(4,"apple"); (10,"pizza")] |> Map
exampleMap.["a"]

let exampleMap3 = Map [("a", 2.0); ("b", 4.0)]
exampleMap3 |> Map.toArray |> Array.averageBy snd

// it will look up the value for the key
Map.find "a" exampleMap
exampleMap2 |> Map.find 10

(**
### Option type
Somtimes something exists or doesn't exist. This can be useful to model explicitly. For example, our `exampleMap` does not have anything associated with key `"c"`. If we try to access `exampleMap.["c"]` we will get an exception. The preferred way to do this is to try to get the key, and then if there is no value for that key return nothing. This is what the option time is useful. Options are either `Some x` or `None`, where `x` is the data that you want. This is what the "..try" functions are about.
*)
// Finds the value, returns the value
Map.tryFind "a" exampleMap
// Doesnt find the value, returns None
Map.tryFind "c" exampleMap

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
// given the number of days and the yield (y)
let priceFromDiscountBasisYield days y =
    // https://www.newyorkfed.org/aboutthefed/fedpoint/fed07.html
    Math.Round(10_000m - (y*100m * decimal days)/360m, 2)

if (priceFromDiscountBasisYield 169 5.08m) <> 9761.52M then
    failwith "Didn't pass FED example"

/// Quick example
(Some 2.0) |> Option.map (fun (x:float) -> x + 1.0)

// Pattern Matching (tells you what you havent covered in your matching)
let f x = 
    match x with
    | 1.0 -> 4.0
    | 2.0 -> 2.0
    | x -> x ** 2.0
f 1.0
f 2.0
f 5.0


// Now calculate T-bill returns and populate our Map.
let dailyRiskFreeRate =
    threeMonthBills.Rows
    |> Seq.choose(fun row ->
        match row.Value with 
        | None -> None
        | Some rate -> Some (row.DATE, rate))
        // row.Value |> Option.map(fun x -> row.DATE, x)) // Choose Non missing observations if its something it will operate on it
    |> Seq.sortBy fst
    |> Seq.pairwise 
    |> Seq.map(fun ((priorDt, priorYld), (dt, yld)) -> 
        // DTB3 is secondary market 3-month yield.
        // So technically *not* constant maturity, 
        // but we'll assume constant and flat term structure. 
        // Flat term structure means yesterday's bonds
        // trade at the same yield as today
        let timeDiff = (dt - priorDt).Days
        let p1 = priceFromDiscountBasisYield 90 priorYld
        let p2 = priceFromDiscountBasisYield (90-timeDiff) yld
        dt, float (p2/p1 - 1m))
    |> Map

// Finding a date on a given day.
Map.tryFind (DateTime(2020,12,31)) dailyRiskFreeRate

// But, we can't find risk-free rates for future days
Map.tryFind (DateTime.Now.AddDays(1.0)) dailyRiskFreeRate
// Or weekends
Map.tryFind (DateTime(2020,12,5)) dailyRiskFreeRate

///////////// NEXT CLASS /////////////

(**

### Recursive functions.
What to do? If we check on a day without a rate, we could check the day before. Here's how to do this using a recursive function. We define recursive functions by putting `rec` in front of them.

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