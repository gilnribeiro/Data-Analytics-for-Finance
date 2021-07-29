// Calculating Returns (Basic Calculations in fsi)

// Let's assume that you have $120.00 today and that you had $100.00 
// a year ago. Your annual return is then:
(120.0/100.0) - 1.0
(120m/100m) - 1m        // Using Decimal
// We use decimals to represent fractions as floats are not really accurate in this matter

// Static Type Checking (We cannot mix data types e.g int float will give error)

// Assign Values:
let yearAgo = 100.0
let today = 120.0
(today/yearAgo) - 1.0

// Define Functions
let calcReturn pv fv = (fv / pv) - 1.0
// The type signature tell us that we get 2 float inputs and map those to a float ouput
// Call the Funtion
calcReturn yearAgo today
calcReturn 100.0 120.0
// However if we try to use decimals this will give an error because we defined the function to operate with floats

// Handling Dividends
let simpleReturn beginningPrice endingPrice dividend = 
    // This is solving for `r` in FV = PV*(1+r)^t where t=1
    (endingPrice + dividend) / beginningPrice - 1.0

let logReturn beginningPrice endingPrice dividend = 
    // This is solving for `r`in FV = PV*e^(rt) where t=1
    log(endingPrice + dividend) - log(beginningPrice)

simpleReturn 100.0 110.0 0.0
logReturn 100.00 110.0 0.0

// My function
let futureValue initialInvest rate period =
    initialInvest*(1.0 + rate)**period

let logFutureValue initialInvest rate period = 
    initialInvest*exp(rate*period)

futureValue 100.0 0.06 5.0
logFutureValue 100.0 0.06 5.0

// Tuples (This is a simple way to group values)
(1,2,3)
(1m,2m)
// Tuples can contain mixed types
(1m, 1)
(1m, 1, 10.0, "2")

// We can also deconstruct tuples
fst (1,2)   // get first value
snd (1,2)   // get second value

// We can also deconstruct using pattern matching
let (a,b) = (1,2)
let (c,d,e) = (1,"2",3.0)

// Now, redifining out simple return funtion to take a single tuple as the input parameter
let simpleReturnTuple (beginningPrice, endingPrice, dividend) = 
    (endingPrice + dividend) / beginningPrice - 1.0

simpleReturnTuple (100.0, 110.0, 0.0)
let xx = (100.0, 120.0, 2.0)
simpleReturnTuple xx

// If we want a more structured tuple we can define a record
type RecordExample = 
    {BeginningPrice: float
     EndingPrice: float
     Dividend: float}
// And construct a value with that record type
let x = {BeginningPrice = 100.0; EndingPrice = 110.0; Dividend = 0.0}

// Similar to tuples we can deconstruct our record value x using pattern matching
let { BeginningPrice = aa; EndingPrice = bb; Dividend = cc } = x

// We can also access individual fields by name
x.EndingPrice / x.BeginningPrice

// We can define a function that operates in on the RecordExample type explicitly
let simpleReturnRecord {BeginningPrice = beginningPrice; EndingPrice = endingPrice; Dividend = dividend} = 
    (endingPrice + dividend) / beginningPrice - 1.0
// or we can let the compiler'stype inference figure out the input type
let simpleReturnRecord2 x = 
    (x.EndingPrice + x.Dividend) / x.BeginningPrice - 1.0
// or we can provide a type hint to tell the compiler the type of the input
let simpleReturnRecord3 (x: RecordExample) = 
    (x.EndingPrice + x.Dividend) / x.BeginningPrice - 1.0

simpleReturnRecord x
simpleReturnRecord2 x
simpleReturnRecord3 x


// WORKING WITH DATA

// Namespaces
// first lets create a file directory to hold data. We are going to use 
// buil-in dotnet IO (input Output) livbraries to do so

    // Set working directory to this code's file directory
// System.IO.Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)
//     // Now create cache directory one level above the working directory
// let cacheDirectory = "../data-cache"
// if not (System.IO.Directory.Exists(cacheDirectory))
// then System.IO.Directory.CreateDirectory(cacheDirectory) |> ignore

// If we want to access the function within the hierarchy without typing the full 
// namespace repetitively we can open it
open System.IO
Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)
let cacheDirectory = "../data-cache"
if not (Directory.Exists(cacheDirectory))
then Directory.CreateDirectory(cacheDirectory) |> ignore


// API KEYS
#load "../secrets.fsx"
// Secrets.tiigoKey

// FSharp.Data is an external library, so we need to download the library from the nuget 
// package manager, which is the primary package manager for the dotnet ecosystem.
#r "nuget: FSharp.Data" // To use a specific version such as 3.3.3 we'd use "nuget: FSharp.Data, 3.3.3"

// Now we're ready to create the web request and cache the results
open System
open System.IO
open FSharp.Data

let tiingoSampleFile = Path.Combine(cacheDirectory, "tiingo-sample.csv")
// only do the request if the file doesn't exist
if not(File.Exists(tiingoSampleFile)) then
    Http.RequestString
            ("https://api.tiingo.com/tiingo/daily/gme/prices", 
            httpMethod = "GET",
            query = [   "token", Secrets.tiigoKey;
                        "startDate", "2020-10-01";
                        "endDate", "2021-02-03";
                        "format","csv"],
            headers = [HttpRequestHeaders.Accept HttpContentTypes.Csv])
    |> fun x -> File.WriteAllText(tiingoSampleFile, x)

// PIPELINES AND LAMBDA EXPRESSIONS
// - pipelines are created using |> and allow us to pipe the output of one functionto the input of another.
// - lambda expressions allow us to create functions on the fly
1.0 |> fun x -> x + 1.0 |> fun x -> x**2.0

// COLECTIONS: ARRAYS, LISTS, SEQUENCES
// read the file return by tiingo
let lines = File.ReadAllLines(tiingoSampleFile)
lines |> Array.take 5
lines.[0]
lines.[0 .. 2]

// A simple float array
let arr = [|1.0 .. 10.0|]
arr.[0]
arr.[0 .. 5]

// list
[ 1.0 .. 10.0]
// sequence
seq { 1.0 .. 10.0}
// Sequences are particularly useful when you have a large, ordered collection 
// of data but don't necessarily expect to use all the elements.
// Individual sequence elements are computed only as required, so a sequence 
// can perform better than a list if not all the elements are used"

// These collections have several built-in funtions for operating on them such
// as map, filter, groupBy etc..
arr
|> Array.map(fun x -> x + 1.0)

arr
|> Array.filter(fun x -> x < 5.0)

arr
|> Array.groupBy(fun x -> x < 5.0)
|> Array.map(fun (group, xs) -> Array.min xs, Array.max xs)

// FSharp.Data Csv Type Provider
// Lets noew process our downloaded data using the FSharp.Data Csv Type Provider.
// This is code that automatically defines the types of input data based on a sample
// since we have already referenced the nuget package and opened the namespace, we can just use it now
let [<Literal>] tiingoSampleFileFullPath = __SOURCE_DIRECTORY__ + "/../data-cache/tiingo-sample.csv"
type TiingoCsv = CsvProvider<tiingoSampleFileFullPath>
let tiingoSample = TiingoCsv.GetSample()

tiingoSample.Rows
|> Seq.map(fun x -> x.Date.ToShortDateString(), x.Close)
|> Seq.take 5
|> Seq.toList

// PLOTTING
#r "nuget: Plotly.NET, 2.0.0-beta5"
open Plotly.NET

let sampleChart =
    tiingoSample.Rows
    |> Seq.map(fun x -> x.Date, x.AdjClose)
    |> Chart.Line
sampleChart |> Chart.Show

// Let's calculate returns for this data. Typically we calculate close-close returns. 
// Looking at the data, we could use the close, divCash, and splitFacor columns to 
// calculate returns accounting for stock splits and dividends (a good at home exercise). 
// But there is also an adjClose column that accounts for both those things. So we we can use this

// RETURNS
let returns =
    tiingoSample.Rows
    |> Seq.sortBy(fun x -> x.Date)
    |> Seq.pairwise
    |> Seq.map(fun (a,b) -> b.Date, calcReturn (float a.AdjClose) (float b.AdjClose))

let avgReturnEachMonth =
    returns
    |> Seq.groupBy(fun (date, ret) -> DateTime(date.Year, date.Month,1))
    |> Seq.map(fun (month, xs) -> month, Seq.length xs, xs |> Seq.averageBy snd)
avgReturnsEachMonth |> Seq.take 3 |> Seq.toList

// The default DateTime printing is too verbose if we don't care about time. We can simplify the printing:
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))
avgReturnEachMonth |> Seq.take 3 |> Seq.toList

let monthlyReturnChart = 
    avgReturnEachMonth
    |> Seq.map(fun (month, cnt, ret) -> month, ret)
    |> Chart.Bar

monthlyReturnChart |> Chart.Show

// VOLATILITY
// We represent volatility by the standar deviation of returns. We can define a std.Dev ourselves
let stddev xs = 
    let mu = xs |> Seq.average
    let sse = xs |> Seq.map(fun x -> (x - mu)**2.0) |> Seq.sum
    let n = xs |> Seq.length |> float
    sqrt (sse/(n - 1.0))

[1.0 .. 10.0] |> stddev 

// or we can use
#r "nuget: FSharp.Stats, 0.4.0"

open FSharp.Stats
[1.0 .. 10.0] |> Seq.stDev

// Lets now look at 5 day rolling volatilities
let rollingVols = 
    returns
    |> Seq.sortBy fst
    |> Seq.windowed 5
    |> Seq.map(fun xs -> 
        let maxWindowDate = xs |> Seq.map fst |> Seq.max
        let dailyVol = xs |> Seq.stDevBy snd
        let annualizedVolInPct = dailyVol * sqrt(252.0) * 100.0
        maxWindowDate, annualizedVolInPct)

let volChart = 
    rollingVols
    |> Chart.Line

volChart |> Chart.Show