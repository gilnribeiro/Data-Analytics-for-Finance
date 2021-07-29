// AQUIRING FAMA_FRENCH DATA
// - Lets aquire a long daily time series on aggregate US Market returns

#r "nuget: FSharp.Data"
open System
open System.Net
open System.IO
open System.IO.Compression
open FSharp.Data

/// URL for the Fama French 3 factor zip archive
let cacheDirectory = Path.Combine(__SOURCE_DIRECTORY__, "../data-cache")
let ff3Url = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
/// Local file path for the Fama French 3 factor zip archive
let ff3ZipFilePath = Path.Combine(cacheDirectory, "ff3.zip")

let web = new WebClient()

web.DownloadFile(ff3Url, ff3ZipFilePath)
web.Dispose()
// Inspect entries
let ff3ZipArchive = ZipFile.OpenRead(ff3ZipFilePath)
ff3ZipArchive.Entries
/// Local csv for Fama French 5 factor data
let ff3Csv = 
    ff3ZipArchive.Entries
    |> Seq.head
    |> fun entry -> Path.Combine(cacheDirectory, entry.Name)

/// This extracts the csv file from the zip file into the directory of choice
// Extract entry
if FileInfo(ff3Csv).LastWriteTime < (DateTime.Now.AddDays(-20.0)) then
    File.Delete(ff3Csv)
ff3ZipArchive.ExtractToDirectory(cacheDirectory)
ff3ZipArchive.Dispose()

let fileLines = File.ReadAllLines(ff3Csv)

fileLines
|> Seq.take 5
|> Seq.toList

// Each element of this array is a line of the file
// This will skip a line until it contains Mkt-Rf
// We also need to take the values up until a point
// So, take while line is not a Blank line ""
fileLines
|> Seq.skipWhile(fun line -> not(line.Contains("Mkt-RF")))
|> Seq.takeWhile(fun line -> line <> "")

// CsvType
// It allows us to create a type for the csv we're in
// what we do, is we give it a sample and it will
// Automatically infer what is the type of the csv, what
// are the columns etc...
type FF3Csv = CsvProvider<"Date (string),Mkt-RF,SMB,HML,RF
19260701,    0.10,   -0.24,   -0.28,   0.009
19260702,    0.45,   -0.32,   -0.08,   0.009
19260706,    0.17,    0.27,   -0.35,   0.009
19260707,    0.09,   -0.59,    0.03,   0.009
19260708,    0.21,   -0.36,    0.15,   0.009">

// This is a "Holder" to hold the csv file since where doing
// Transformations on it - this is a record type
type FF3Obs = 
    { Date : DateTime 
      MktRf : float
      Smb : float 
      Hml : float
      Rf : float }


// Some info on date parsing [here](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=net-5.0). But if we were ever going to save this date data to disk or work with times, we should prefer [DateTimeOffset](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime) or [NodaTime](https://nodatime.org/).
let dateParse x = 
    DateTime.ParseExact(x,
        "yyyyMMdd",
        Globalization.CultureInfo.InvariantCulture)
dateParse("20200104")

// we're going to create an array of the file ff3
// Just taking the number files (skipping the header)
let ff3 =
    fileLines
    |> Array.skipWhile(fun line -> not (line.Contains("Mkt-RF")))
    |> Array.skip 1
    |> Array.takeWhile(fun line -> line <> "")
    |> Array.map(fun line -> 
        let parsedLine = FF3Csv.ParseRows(line).[0] 
        { Date = dateParse parsedLine.Date
          MktRf = float parsedLine.``Mkt-RF`` / 100.0
          Smb = float parsedLine.SMB / 100.0
          Hml = float parsedLine.HML / 100.0
          Rf = float parsedLine.RF / 100.0 })
        
// whenever you see a date type, print it into a more compact version
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

ff3 |> Seq.take 5

// OBSERVING TIME-VARYING VOLATILITY
// Volatily tends to be somewhat persistent. By this, we mean that if our risky asset is volatile
// today, then it is likely to be volatile tomorrow
// This means we can use past volatility to form estimates of future volatility

#r "nuget: FSharp.Stats, 0.4.0"
#r "nuget: Plotly.NET, 2.0.0-beta5"
open FSharp.Stats
open Plotly.NET

let annualizeDaily x = x * sqrt(252.0) * 100.0

let monthlyVol = 
    ff3
    // Sort by date
    |> Seq.sortBy(fun x -> x.Date)
    // group by year and month
    |> Seq.groupBy(fun x -> x.Date.Year, x.Date.Month)
    // sequence it by yearmonth and return the last day for each month
    |> Seq.map(fun (_ym, xs) ->
        let dt = xs |> Seq.last |> fun x -> x.Date
        let annualizedVolPct = xs |> stDevBy(fun x -> x.MktRf) |> annualizeDaily
        dt, annualizedVolPct)
    |> Seq.toArray

// Create a function to create column charts
let volChart vols = 
    let getYear f = vols |> Seq.map(fun (dt:DateTime, _vol) -> dt.Year ) |> f
    let minYear = getYear Seq.min
    let maxYear = getYear Seq.max
    vols
    |> Chart.Column
    |> Chart.withTraceName $"Time-varying Volatility ({minYear}-{maxYear})"
    |> Chart.withY_AxisStyle(title = "Annualized Volatility (%")

// // Using it
volChart monthlyVol
|> Chart.Show

let allVolsChart = volChart monthlyVol
let since2019VolChart = 
    monthlyVol
    |> Seq.filter(fun (dt, _) -> dt >= DateTime(2019,1,1))
    |> volChart

allVolsChart |> Chart.Show
since2019VolChart |> Chart.Show
 
// REVIEW OF CALCULATING PORTFOLIO WEIGHTS
// - Portfolio weights (position value)/(portfolio value)
// - A long portfolio has portfolio wights that sum up to 1.0 or (100%)
// - A zero-cost porfolio has portfolio wights that sum up to 0.0

type Position = {Id: string; Position: int; Price: decimal}

let portfolio =
    [|  { Id = "AAPL"; Position = 100; Price = 22.20m }
        { Id = "AMZN"; Position = 20; Price = 40.75m }
        { Id = "TSLA"; Position = 50; Price = 30.6m } |]

// To Do it Properly
type PositionValue = {Id: string; Value: float}
type PositionWeight = {Id: string; Weight: float}

let calcValue (x:Position) = 
    { Id = x.Id
      Value = (float x.Position) * (float x.Price)}

let calcWeights xs = 
    let portfolioValue = xs |> Array.sumBy(fun x -> x.Value)
    xs
    |> Array.map(fun pos ->
        {   Id = pos.Id
            Weight = pos.Value / portfolioValue})

portfolio
|> Array.map calcValue
|> calcWeights

let portfolioWithShorts =
    [|  { Id = "AAPL"; Position = 100; Price = 22.20m }
        { Id = "AMZN"; Position = -20; Price = 40.75m }
        { Id = "TSLA"; Position = 50; Price = 30.6m } |]

portfolioWithShorts
|> Array.map calcValue
|> calcWeights

// EFFECT OF LEVERAGE ON VOLATILITY
// If one asset is the risk free asset (borrowing a risk-free bond), then this asset has no variance 
// and the covariance term is 0.0. Thus we are left with the result
// that if we leverage asset by borrowing or lending the risk-free asset, then our leveraged portfolio's standard deviation ( ) is
// stdev = w(x) * stdev(x)

let leveragedVol (weight, vol) = weight * vol

let exampleLeverages = [1.0; 1.5; 2.0]

let rollingVolSubset = 
    // Get monthly volatility
    monthlyVol
    |> Seq.filter(fun (dt, vol) -> dt > DateTime(2020,1,1))

let exampleLeveragesVols = 
    exampleLeverages
    |> Seq.map(fun leverage ->
        leverage, 
        rollingVolSubset
        |> Seq.map(fun (dt, vol) -> dt, leveragedVol(leverage, vol))) 
        |> Seq.toArray
    |> Seq.toArray

// This is a way to do the above but instead of using anonymous functions we are defining
let exampleLeveragedVols2 = 
    let calcVolsForLeverage leverage = 
        let getLeveragedMonth (dt: DateTime, monthlyVol:float) = 
            dt, leveragedVol(leverage, monthlyVol)
        leverage, rollingVolSubset |> Seq.map getLeveragedMonth
    exampleLeverages |> Seq.map calcVolsForLeverage

let exampleLeveragesChart = 
    exampleLeveragesVols
    |> Seq.map(fun (leverage, leverageVols) -> 
        leverageVols
        |> Chart.Line
        |> Chart.withTraceName $"Leverage of {leverage}")
    |> Chart.Combine
exampleLeveragesChart |> Chart.Show

// EFFECT OF LEVERAGE ON RETURNS
// if we borrow 50% of our starting equity by getting a RF loan, then we have
// - Basically:  r(levered) = 150% * r(unlevered) - 50% * rf
// in excess returns
// r(levered) - rf = 150% * (r(unlevered) * rf)
// So if we work in excess returns we can just multiply unlevered excess returns by the weight

let invest = 1.0m
let borrow = 1.0m
let ret = 0.15m
let result = (invest * borrow)*(1.0m + ret)*borrow
result = 1.0m + ret * (invest + borrow)/invest

let leveredReturn leverage (x: FF3Obs) = leverage * x.MktRf

// We can illustrate this with cumulative return plots. Let's first 
// show a simple example for how we can calculate cumulative returns.
// Imagine that you invest $1 at a 10% return. What do you have after n years?
[|1.0 .. 5.0|]
|> Array.map(fun years -> 1.0*((1.0+ 0.1)**years))
// What if we had data like this?
[|for i = 1 to 5 do 0.1|] // This repeats the same value 5 times, like a list comprehension

// If we look at the definition of `mapFold`, we can see that we call it with `Array.mapFold mapping state array`, where 

// ```output
// mapping : 'State -> 'T -> 'Result * 'State
// The function to transform elements from the input array and accumulate the final value.

// state : 'State
// The initial state.

// array : 'T[]
// The input array.

// Returns: 'Result[] * 'State
// The array of transformed elements, and the final accumulated value.
// ```

// MAPFOLD
(1.0, [| 0.1; 0.2; -0.1|])
||> Array.mapFold (fun state x -> string x, state * (1.0 + x))

(1.0, [| 0.1; 0.2; -0.1|])
||> Array.mapFold (fun state x -> x+2.0, state * (1.0 + x))

// val scan:
//     folder: 'State -> 'T -> 'State ->
//     state : 'State   ->
//     array : 'T array
//         -> 'State array

// This is the compounding of 1 dollar for 3 years
(1.0, [| 0.1; 0.2; -0.1|])
||> Array.scan (fun state x -> state * (1.0 + x))
// |> Array.tail This gives us the array minus the first value
// |> Array.map(fun x -> x - 1.0) to get the interest

let exampleMapper inCumRet ret = 
    let outCumRet = inCumRet * (1.0 + ret)
    outCumRet, outCumRet

let exampleMapper2 inCumRet ret = 
    let outCumRet = inCumRet * (1.0 + ret)
    outCumRet - 1.0, outCumRet

let exampleInitialState = 1.0 // Your 1$ at the start
let exampleArrayToAccumulate = [|for i = 1 to 5 do 0.1|]
Array.mapFold exampleMapper exampleInitialState exampleArrayToAccumulate
// or
exampleArrayToAccumulate |> Array.mapFold exampleMapper exampleInitialState
// relative to a 0 starting point
exampleArrayToAccumulate |> Array.mapFold exampleMapper2 exampleInitialState

// Thats a cumulative return! We can get rid of the cumulative state in the second part
// of the tuple with a fst all since it is a tuple of pairs
exampleArrayToAccumulate
|> Array.mapFold exampleMapper2 exampleInitialState
|> fst

// Now we can calculate cumulative returns of our data
let since2020 = 
    ff3 |> Seq.filter(fun x -> x.Date >= DateTime(2020,1,1))
    
let cumulativeReturnEx =
    let mapping inCumRet x = 
        let outCumRet = inCumRet * (1.0 + x.MktRf)
        // create a new record with the copy of x 
        { x with MktRf = outCumRet - 1.0}, outCumRet
    
    (1.0, since2020)
    ||> Seq.mapFold mapping 
    |> fst

cumulativeReturnEx |> Seq.sortByDescending(fun x -> x.Date) |> Seq.take 5

let cumulativeReturnExPlot =
    cumulativeReturnEx
    |> Seq.map(fun x -> x.Date.Date, x.MktRf)
    |> Chart.Line

cumulativeReturnExPlot |> Chart.Show

// Not cumulative
since2020
|> Seq.map(fun x -> x.Date.Date, x.MktRf)
|> Chart.Line
|> Chart.Show

// Lets try leverage with daily rebalancing (each day we take leverage of X)
let getLeveragedReturn leverage =
    let mapping inCumRet x = 
        let lr = leveredReturn leverage x
        let outCumRet = inCumRet * (1.0 + lr)
        { x with MktRf = outCumRet - 1.0}, outCumRet
    
    (1.0, since2020)
    ||> Seq.mapFold mapping 
    |> fst
    |> Seq.map(fun x -> x.Date.Date, x.MktRf)

let exampleLeveragedReturnChart = 
    exampleLeverages
    |> Seq.map(fun lev ->
        getLeveragedReturn lev
        |> Chart.Line
        |> Chart.withTraceName $"Leverage of {lev}") 
    |> Chart.Combine

exampleLeveragedReturnChart |> Chart.Show


/////////////////////////
    /// Going Over Mapfold
////////////////////////
let observation = ("AAPL", 1.0)
let mapfun (acc: float) ((id, ret):string * float) =
    let cumulativeReturn = acc * (1.0 + ret)
    (id, cumulativeReturn)
////// init state // observation
mapfun 10.0 ("AAPL", 0.3)

let foldfun (acc: float) ((id, ret):string * float) =
    acc*(1.0 + ret)

foldfun 10.0 ("AAPL", 0.3)

let mapfoldfun acc y = mapfun acc y, foldfun acc y
(10.0, [("AAPL", 0.3); ("AAPL", 0.3)])
||> List.mapFold mapfoldfun

////////////////////////
 
[2 .. 10]
|> List.takeWhile (fun x -> x < 2)

////////////////////////
    /// Class
////////////////////////
