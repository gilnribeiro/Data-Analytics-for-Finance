#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET, 2.0.0-beta5"
open System
open FSharp.Data
open FSharp.Stats
open Plotly.NET

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

// Get 10 year rates
let gs10 = CsvProvider<"https://fred.stlouisfed.org/graph/fredgraph.csv?id=GS10">.GetSample()

gs10.Rows
|> Seq.map(fun x -> x.DATE, x.GS10)
|> Chart.Line
|> Chart.withTitle "10-year rate"
|> Chart.Show

// Get 20 year rates
let gs20 = CsvProvider<"https://fred.stlouisfed.org/graph/fredgraph.csv?id=GS20">.GetSample()

gs20.Rows
|> Seq.map(fun x -> x.DATE, x.GS20)
|> Chart.Line
|> Chart.withTitle "20-year rate"
|> Chart.Show

// Group 20 year bond by yaer and find the month with the minimum interest rate
gs20.Rows
|> Seq.groupBy(fun x -> x.DATE.Year)
|> Seq.map(fun (year, rowsInTheYear) ->
    year, rowsInTheYear |> Seq.minBy (fun row -> row.GS20))

gs10.Rows
|> Seq.groupBy (fun row -> row.DATE.Year, row.DATE.Month)
|> Seq.map (fun (year, rowsInTheYear) -> 
    year, rowsInTheYear |> Seq.maxBy (fun row -> row.GS10))

// Schema parameter overrides the column headers
// Missing values as "." -> Option Type

// Option Type
type OptionRecord = {Value : string Option}
let x = {Value = Some "1.0"}
let y = {Value = None}
let parseOptionStringToFloat (x: string Option) =
    x |> Option.map(fun x -> float x)

parseOptionStringToFloat x.Value
parseOptionStringToFloat y.Value

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

[Some 1.0; Some 2.0; None; Some 4.0]
|> List.choose (fun x -> x)

// Im going to create a list of tuples (so, the fred code of the list  i want to download)
let chart1y10y = 
    ["GS1", "1-Year"
     "GS10", "10-Year"]
    |> Seq.map(fun (series, name) -> 
        Fred.Load(fredUrl series).Rows
        |> Seq.choose(fun row -> 
            match row.Value with
            | None -> None
            | Some rate -> Some (row.DATE, rate))
        |> Chart.Line
        |> Chart.withTraceName name)
    |> Chart.Combine
    |> Chart.withTitle "Treasury Constant Maturity Rates"
    |> Chart.withY_AxisStyle(title = "Interest Rate (%)")
 
 
chart1y10y |> Chart.Show

// Self-check: Can you plot the USD/Euro exchange rate?

let chart2 = 
    ["DEXUSEU", "EU-US"]
    |> Seq.map(fun (series, name) ->
        Fred.Load(fredUrl series).Rows
        |> Seq.choose (fun row -> 
        match row.Value with
        | None -> None
        | Some rate -> Some (row.DATE, rate))
        |> Chart.Line
        |> Chart.withTraceName name)
    |> Chart.Combine
    |> Chart.withTitle "EU US Exchange Rate"
    |> Chart.withY_AxisStyle(title="Interest Rate (%)")

chart2 |> Chart.Show

// Teachers Example
let euro = Fred.Load(fredUrl "DEXUSEU")
euro.Rows
|>  Seq.choose (fun x -> 
    match x.Value with
    | None -> None
    | Some exchangerate -> Some (x.DATE, exchangerate))
|> Chart.Line
|> Chart.Show

// A good short term risk-free rate is US t-bills
// Lets take a look at a few recent days
let threeMonthBills = Fred.Load(fredUrl "DTB3")
threeMonthBills.Rows
|> Seq.sortByDescending (fun x -> x.DATE)
|> Seq.take 10
|> Seq.toList

// Map Collections
// We want to be able to look up a risk-free rate on a given day. If we're 
// doing lookups, then a good data structure for that is a map. Maps 
// consist of key and value pairs. If you look up the key, you get the value. 
// If you need to do lookups on a key, maps are much more efficient than 
// trying to do the same thing with a list or array. Some more info 
// [here](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-mapmodule.html).

// Basically an immutable dictionary
let exampleMap = Map [("a", "hey");("b","ho")]
exampleMap.["a"]

let exampleMap2 = Map [(4, "apple");(10, "pizza")]
exampleMap2.[4]

let exampleMap3 = Map [("a", 2.0);("b", 4.0)]
exampleMap3.["a"]

// it will look up the value for the key
Map.find "a" exampleMap
exampleMap2 |> Map.find 10


// Comparing perfomance of array vs. Map Lookups~
#time "on"
let isOdd x = 
    if x % 2 = 0 then
        false
    else
        true
let arr = [| for i = 1 to 100_000 do (i, isOdd i) |]
let arrMap = arr |> Map

arr |> Array.find (fun (a,b) -> a = 100)
arrMap |> Map.find 101

// Compare performance to find something at the beginning of an array
for i = 1 to 100 do
    arr |> Array.find(fun (a,b) -> a = 1_000) |> ignore

for i = 1 to 100 do
    arrMap |> Map.find 1_000 |> ignore

// Option Type
// Somtimes something exists or doesn't exist. This can be useful to model 
// explicitly. [FSharp For Fun And Profit](https://fsharpforfunandprofit.com/posts/the-option-type/) 
// has a nice discussion of option types and how to use them. 
// You can also find information on the [tour of F#](https://docs.microsoft.com/en-us/dotnet/fsharp/tour#optional-types) and the [F# language reference](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-core-optionmodule.html).

// The main purpose is to model situations where you could have 
// "something" or "nothing" explicitly.

// For example, our `exampleMap` does not have anything associated with 
// key `"c"`. If we try to access `exampleMap.["c"]` we will get an 
// exception. The preferred way to do this is to try to get the key, 
// and then if there is no value for that key return nothing. 
// This is what the option time is useful. Options are either 
// `Some x` or `None`, where `x` is the data that you want. 
// This is what the "..try" functions are about.

Map.tryFind "a" exampleMap
Map.tryFind "c" exampleMap

// Other option examples
let xx = Some 4.0
let yy = None

xx |> Option.map( fun x -> x + 1.0)
yy |> Option.map( fun x -> x + 1.0)

let xxyy = [xx; yy; xx; yy; xx |> Option.map(fun x -> x + 100.0)]

xxyy

let divideBy2 x = x/2.0
xxyy
|> List.map ( fun x -> x |> Option.map(fun x -> divideBy2 x))
xxyy
|> List.map(fun x -> 
    x |> Option.map divideBy2)

// Estimating a bond return from changing interest rates
// Now, that we have a data structure that we can use for interest rate 
// look-ups, we want to populate it with returns on risk-free assets.

// The T-bill rates that we looked at just now were interest rates quoted on
// a particular flavor of annual basis. We don't want the annual interest 
// rate.  We want the return on the risk-free asset over a particular period. 
// T-bills are zero-coupon bonds, so we can calculate the return based on 
// the changing price.

// To do so, we need to use the interest rate to obtain the price.

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

let threeMonthBills = Fred.Load(fredUrl "DTB3")
threeMonthBills.Rows

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

// Fiding a rate on a given day
Map.tryFind (DateTime(2020, 12, 31)) dailyRiskFreeRate

// But can't find risk free rates for future days
Map.tryFind (DateTime.Now.AddDays(1.0)) dailyRiskFreeRate
// or weekends
Map.tryFind (DateTime(2020,12,5)) dailyRiskFreeRate

// More pattern matching 
let f1 x =
    match x with // x is the thing that I am matching
    | 1.0 -> 1.0 // when x matches 1.0 then return 1.0
    | 2.0 -> 1.0 // when x matches with 2.0 then return 1.0
    | 3.0 -> 7.0 + 7.0 // when x matches with 3.0 return 7.0+7.0
    | y -> y ** 2.0 

[1.0 .. 10.0] |> List.map f1

// Recursive Functions
// Ok, now what do we do if we look for an interest rate on a weekend? 
// If we check on a day without a rate, we could check the day before. 
// Here's how to do this using a recursive function. We define recursive 
// functions by putting `rec` in front of them.

let rec tempGetRate (date:DateTime) = 
    // first, try to find the risk free rate on our date
    match Map.tryFind date.Date dailyRiskFreeRate with 
    | Some rate -> date, rate
    | None -> tempGetRate (date.AddDays(-1.0))

// Check a weekend
tempGetRate (DateTime(2020, 12, 5))
tempGetRate (DateTime(2020, 12, 6))    

// Any gotchas? Well, if the date we check is earlier than our first 
// risk-free rate, then we will keep checking forever and never 
// find anything. Probably, we want to stop checking after some point.

/// tries to find the latest risk free rate on or before a given date

let getDailyRiskFree (date:DateTime) =
    let rec loop (innerDate:DateTime) =
        match Map.tryFind innerDate.Date dailyRiskFreeRate with
        | Some rate -> rate
        | None ->
            // Here we see the check
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

getDailyRiskFree (DateTime(1893, 12, 6))