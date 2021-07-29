(**
# Some additional Tiingo download examples.
The file `common.fsx` should be in the same directory as `secrets.fsx`. 
The file `test.fsx` should be one level deeper inside a folder, like "MyPortfolio". 
So if secrets and common are in your `class` folder, then test would be in `class/MyPortfolio`.
*)

#r "nuget: FSharp.Data"
#r "nuget: Plotly.NET,2.0.0-beta6"
#load "../secrets.fsx"
      "../common.fsx"

open System
open Common
open Plotly.NET

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

// Testing some requests
let testRequest = Tiingo.request "MSFT"      
// look at it
testRequest

// Other requests
let testRequest2 = testRequest |> Tiingo.startOn (DateTime.Now.AddDays(-5.0))

let testRequest3 =
    "AMD"
    |> Tiingo.request
    |> Tiingo.startOn (DateTime(2020,12,31))
    |> Tiingo.endOn  (DateTime(2021,2,15))

let testResult = Tiingo.get testRequest
// look at it
testResult

testResult
|> Seq.map(fun x -> x.Date,x.Close)
|> Chart.Line
|> Chart.Show

let testLastObs =
    testResult
    |> Seq.sortBy(fun x -> x.Date)
    |> Seq.last
// High, Low, and Volume?
testLastObs.High
testLastObs.Low
testLastObs.Volume

type SymbolPrice = 
    { Symbol : string 
      Date : DateTime 
      Price : float }
// Function to get last price
let lastPrice symbol =
    let tiingoRequest = symbol |> Tiingo.request |> Tiingo.startOn (DateTime.Now.AddDays(-14.0))
    let tiingoResult = Tiingo.get tiingoRequest
    let lastRow = 
        tiingoResult
        |> Seq.sortBy(fun row -> row.Date)
        |> Seq.last
    { Symbol = symbol
      Date = lastRow.Date
      Price = float lastRow.Close }

// check
lastPrice "MSFT"

// for all
let tickers = [| "MSFT";"GM";"AAPL" |]

tickers
|> Array.map lastPrice

let allReturns =
    tickers
    |> Array.map Tiingo.request
    |> Array.collect Tiingo.getReturns

// look at it
allReturns

let maxReturnDay =
    allReturns
    |> Array.groupBy(fun x -> x.Symbol)
    |> Array.map(fun (_symbol, returnObs) -> 
        returnObs 
        |> Array.maxBy(fun day -> day.Return) )

// look at it        
maxReturnDay

let cumReturn (stock, returnDays: ReturnObs array) =
    let mapfolder priorReturn today =
        let newReturn = priorReturn * (1.0 + today.Return)
        { today with Return = newReturn - 1.0 }, newReturn
    let sortedDays = returnDays |> Array.sortBy(fun x -> x.Date)
    (1.0, sortedDays)
    ||> Array.mapFold mapfolder
    |> fst

let testStock = "MSFT", allReturns |> Array.filter(fun x -> x.Symbol = "MSFT")

// look at it
testStock

// look at it
cumReturn testStock

// plot it
cumReturn testStock
|> Array.map(fun x -> x.Date, x.Return )
|> Chart.Line
|> Chart.Show

/// General plotting function.
/// Stock is any string, return days is a `ReturnObs []`.
let makeCumulativeReturnPlot (stock, returnDays) =
    let cr = cumReturn (stock, returnDays)
    
    cr
    |> Array.map(fun x -> x.Date, x.Return )
    |> Chart.Line
    |> Chart.withTraceName stock

// look at it
testStock
|> makeCumulativeReturnPlot
|> Chart.Show

let twoStocks =
    [| "MSFT", allReturns |> Array.filter(fun x -> x.Symbol = "MSFT")
       "GM", allReturns |> Array.filter(fun x -> x.Symbol = "GM") |]

// make sure you know what the array of two stocks looks like.
// It is an array of two elements. Each element is a tuple pair with
// the symbol as the first thing and the return observations as the second thing.

let first3Days (symbol, returnDays) =  symbol, returnDays |> Array.take 3

twoStocks.[0] |> first3Days
twoStocks.[1] |> first3Days

// same using anonymous function
twoStocks.[0] |> fun (symbol, returnDays) -> symbol, returnDays |> Array.take 3
twoStocks.[1] |> fun (symbol, returnDays) -> symbol, returnDays |> Array.take 3

// Doing it to the whole array
twoStocks
|> Array.map first3Days

// same using anonymous function
twoStocks
|> Array.map (fun (symbol, returnDays) -> symbol, returnDays |> Array.take 3)

// An array of two "GenericCharts".
// The charts won't "show" until we Chart.Show them
let twoCharts =
    twoStocks
    |> Array.map makeCumulativeReturnPlot

// look at it, an array of two charts.
twoCharts

// plotting both manually
// You should see two browser tabs open with one chart for each stock.
twoCharts.[0] |> Chart.Show
twoCharts.[1] |> Chart.Show

// for loop way to do it.
for chart in twoCharts do
    chart |> Chart.Show

// Array.iter way to do it
twoCharts 
|> Array.iter Chart.Show // Use iter instead of map because Chart.Show returns unit

// Now two charts on the same tab.
twoCharts
|> Chart.Combine
|> Chart.Show
