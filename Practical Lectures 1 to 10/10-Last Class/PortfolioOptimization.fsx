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
The tangency portfolio is the portfolio fully invested 
in risky assets that has the maximum achievable sharpe ratio. 
When there is a risk-free rate, the efficient frontier 
of optimal portfolios is some combination of
the tangency portfolio and the risk-free asset. 
Investors who want safe portfolios hold a lot 
of bonds and very little of the tangency portfolio. 
Investors who want riskier portfolios hold little risk-free bonds and
a lot of the tangency portfolio (or even lever the tangency portoflio). 

Now one thing to keep in mind is that often you think 
of this as the optimal weight per security.
But one well known problem is that trying to do this naively does not work well.
And by naively I mean taking a stock's average return and covariances in the sample. 
In large part, this is because it is hard to estimate a stock's past returns.
I know. Big shock, right?

However, there are ways to do portfolio optimization that works better.
We can do it by creating large groups 
of stocks with similar characteristics. 
For example, a factor portfolio. 
Then you estimate the expected return and covariance matrix using the factor.
That tends to give you better portfolios 
because the characteristics that you're using 
to form the portfolios help you estimate 
the return and covariances of the stocks in it.
A type to hold our data.
*)
type StockData =
    { Symbol : string 
      Date : DateTime
      Return : float }
(**
We get the Fama-French 3-Factor asset pricing model data.
*)
let ff3 = French.getFF3 Frequency.Monthly

// Transform to a StockData record type.
let ff3StockData =
    [| 
       ff3 |> Array.map(fun x -> {Symbol="HML";Date=x.Date;Return=x.Hml})
       ff3 |> Array.map(fun x -> {Symbol="MktRf";Date=x.Date;Return=x.MktRf})
       ff3 |> Array.map(fun x -> {Symbol="Smb";Date=x.Date;Return=x.Smb})
    |] |> Array.concat
(**
Let's get our factor data.
*)
let myFactorPorts = CsvProvider<"myExcessReturnPortfolios.csv",
                                ResolutionFolder = __SOURCE_DIRECTORY__>.GetSample()

let long = 
    myFactorPorts.Rows 
    |> Seq.filter(fun row -> row.PortfolioName = "Mine" && row.Index = Some 3)
    |> Seq.map(fun x -> { Symbol = "Long"; Date = x.YearMonth; Return = x.Ret })
    |> Seq.toArray
(**
Some good standard investments.
*)
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

Remember how to do this?
*)
let exampleRets = [| 0.1; 0.1; 0.1 |]

(1.0, exampleRets)
||> Array.fold (fun acc ret -> acc * (1.0 + ret))
|> fun grossReturn -> grossReturn - 1.0(* output: 
0.331*)
// Compare to 
1.1**3.0 - 1.0
(**
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
(**
And let's convert to excess returns
*)
let rf = ff3 |> Seq.map(fun x -> x.Date, x.Rf) |> Map

let standardInvestmentsExcess =
    let maxff3Date = ff3 |> Array.map(fun x -> x.Date) |> Array.max
    standardInvestments
    |> Array.filter(fun x -> x.Date <= maxff3Date)
    |> Array.map(fun x -> 
        match Map.tryFind x.Date rf with 
        | None -> failwith $"why isn't there a rf for {x.Date}"
        | Some rf -> { x with Return = x.Return - rf })
(**
If we did it right, the `VTI` return should be pretty similar to the `MktRF`
return from Ken French's website.
*)
standardInvestmentsExcess
|> Array.filter(fun x -> x.Symbol = "VTI" && x.Date.Year = 2021)
|> Array.map(fun x -> x.Date.Month, round 4 x.Return)
|> Array.take 3(* output: 
val it : (int * float) [] = [|(1, -0.0033); (2, 0.0314); (3, 0.0365)|]*)
ff3 
|> Array.filter(fun x -> x.Date.Year = 2021)
|> Array.map(fun x -> x.Date.Month, round 4 x.MktRf)(* output: 
val it : (int * float) [] = [|(1, -0.0003); (2, 0.0278); (3, 0.0309)|]*)
(**
Let's put our stocks in a map keyed by symbol
*)
let stockData =
    Array.concat [| standardInvestmentsExcess; long |]
    |> Array.groupBy(fun x -> x.Symbol)
    |> Map

let symbols = 
    stockData 
    |> Map.toArray // convert to array of (symbol, observations for that symbol) 
    |> Array.map fst // take just the symbol
    |> Array.sort // sort them
(**
Let's create a function that calculates covariances
for two securities.
*)
let getCov x y stockData =
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

let covariances =
    symbols
    |> Array.map(fun x ->
        symbols
        |> Array.map(fun y -> getCov x y stockData))
    |> matrix
let means = 
    stockData
    |> Map.toArray
    |> Array.map(fun (sym, xs) ->
        sym,
        xs |> Array.averageBy(fun x -> x.Return))
    |> Array.sortBy fst
    |> Array.map snd
    |> vector
(**
This solution method for finding the tangency portfolio
comes from Hilliar, Grinblatt and Titman 2nd
European Edition, Example 5.3. 

Since it has the greatest possible Sharpe ratio, that means
that you cannot rebalance the portfolio and increase 
the return per unit of standard deviation.

The solution method relies on the fact that covariance
is like marginal variance. At the tangency portfolio,
it must be the case that the ratio of each asset's 
risk premium to it's covariance with the tangency portfolio, 
$(r_i-r_f)/cov(r_i,r_p)$, must be the same. Because that's
the return per marginal variance ratio, and if it was not
equal for all assets, then you could rebalance and increase the portfolio's 
return while holding the portfolio variance constant.

In the below algebra, we solve for the portfolio that has covariances with
each asset equal to the asset's risk premium. Then we relever to have a portfolio
weight equal to 1.0 (we can relever like this because everything is in excess returns)
and then we are left with the tangency portfolio.
*)
// solve A * x = b for x
let w' = Algebra.LinearAlgebra.SolveLinearSystem covariances means
let w = w' |> Vector.map(fun x -> x /  Vector.sum w')
w(* output: 
vector [|0.8662429416; 0.136558377; -0.002801318541|]*)
(**
Portfolio variance
*)
let portVariance = w.Transpose * covariances * w
(**
Portfolio standard deviation
*)
let portStDev = sqrt(portVariance)
(**
Portfolio mean 
*)
let portMean = w.Transpose * means
(**
Annualized Sharpe ratio 
*)
sqrt(12.0)*(portMean/portStDev)(* output: 
1.046837944*)
(**
## Comparing mean-variance efficient to 60/40.

Now let's form the mean-variance efficient portfolios
based on the above optimal weights and compare them to
a 60/40 portfolio over our sample. A 60% stock and 40%
bond portfolio is a common investment portfolio.

Our weights are sorted by `symbols`. Let's put them into a
Map collection for easier referencing.
*)
let weights =
    Seq.zip symbols w
    |> Map.ofSeq
(**
Next, we'd like to get the symbol data grouped by date.
*)
let stockDataByDate =
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

Compare the first month of data
*)
let firstMonth =
    stockDataByDate 
    |> Array.head // first date group
    |> snd // convert (date, StockData array) -> StockData array
// look at it
firstMonth(* output: 
[|{ Symbol = "Long"
    Date = 2/1/2000 12:00:00 AM
    Return = 0.09618188163 }|]*)
// How many stocks?
firstMonth.Length(* output: 
1*)
(**
to the last month of data
*)
// Last item
let lastMonth =
    stockDataByDate 
    |> Array.last // last date group
    |> snd // convert (date, StockData array) -> StockData array
// look at it
lastMonth(* output: 
[|{ Symbol = "BND"
    Date = 3/1/2021 12:00:00 AM
    Return = -0.012714944 }; { Symbol = "VTI"
                               Date = 3/1/2021 12:00:00 AM
                               Return = 0.03645193422 }|]*)
// How many stocks?
lastMonth.Length(* output: 
2*)
(**
What's the first month when you have all 3 assets?
*)
let allAssetsStart =
    stockDataByDate
    // find the first array element where there are as many stocks as you have symbols
    |> Array.find(fun (month, stocks) -> stocks.Length = symbols.Length)
    |> fst // convert (month, stocks) to month

let allAssetsEnd =
    stockDataByDate
    // find the last array element where there are as many stocks as you have symbols
    |> Array.findBack(fun (month, stocks) -> stocks.Length = symbols.Length)
    |> fst // convert (month, stocks) to month
(**
Ok, let's filter our data between those dates.
*)
let stockDataByDateComplete =
    stockDataByDate
    |> Array.filter(fun (date, stocks) -> 
        date >= allAssetsStart &&
        date <= allAssetsEnd)
(**
Double check that we have all assets in all months for this data.
*)
stockDataByDateComplete
|> Array.map snd
|> Array.filter(fun x -> x.Length <> symbols.Length) // discard rows where we have all symbols.
|> fun filteredResult -> 
    if not (Array.isEmpty filteredResult) then 
        failwith "stockDataByDateComplete has months with missing stocks"
(**
Now let's make my mve and 60/40 ports

To start, let's take a test month so that it is easy to see
what we are doing.
*)
let testMonth =
    stockDataByDateComplete
    |> Array.find(fun (date, stocks) -> date = allAssetsStart)
    |> snd

let testBnd = testMonth |> Array.find(fun x -> x.Symbol = "BND")
let testVti = testMonth |> Array.find(fun x -> x.Symbol = "VTI")
let testLong = testMonth |> Array.find(fun x -> x.Symbol = "Long")

testBnd.Return*weights.["BND"] +
testVti.Return*weights.["VTI"] +
testLong.Return*weights.["Long"](* output: 
0.004034528203*)
// Or, same thing but via iterating through the weights.
weights
|> Map.toArray
|> Array.map(fun (symbol, weight) ->
    let symbolData = testMonth |> Array.find(fun x -> x.Symbol = symbol)
    symbolData.Return*weight)
|> Array.sum    (* output: 
0.004034528203*)
(**
Now in a function that takes weights and monthData as input
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
    
portfolioMonthReturn weights testMonth
(**
Here's a thought. We just made a function that takes
weights and a month as input. That means that it should
work if we give it different weights.

Let's try to give it 60/40 weights.
*)
let weights6040 = Map [("VTI",0.6);("BND",0.4)]

weights6040.["VTI"]*testVti.Return +
weights6040.["BND"]*testBnd.Return(* output: 
0.02104444042*)
(**
Now compare to
*)
portfolioMonthReturn weights6040 testMonth(* output: 
0.02104444042*)
(**
Now we're ready to make our mve and 60/40 portfolios.
*)
let portMve =
    stockDataByDateComplete
    |> Array.map(fun (date, data) -> 
        { Symbol = "MVE"
          Date = date
          Return = portfolioMonthReturn weights data })

let port6040 = 
    stockDataByDateComplete
    |> Array.map(fun (date, data) -> 
        { Symbol = "60/40"
          Date = date 
          Return = portfolioMonthReturn weights6040 data} )
(**
It is nice to plot cumulative returns.
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
Ok, cumulative returns.
*)
let portMveCumulative = 
    portMve
    |> cumulateReturns

let port6040Cumulative = 
    port6040
    |> cumulateReturns


let chartMVE = 
    portMveCumulative
    |> Array.map(fun x -> x.Date, x.Return)
    |> Chart.Line
    |> Chart.withTraceName "MVE"

let chart6040 = 
    port6040Cumulative
    |> Array.map(fun x -> x.Date, x.Return)
    |> Chart.Line
    |> Chart.withTraceName "60/40"

let chartCombined =
    [| chartMVE; chart6040 |]
    |> Chart.Combine
chartCombined |> Chart.Show
(**
Those are partly going to differ because they have different volatilities.
It we want to have a sense for which is better per unit of volatility,
then it can make sense to normalize volatilities.
First compare the MVE vol
*)
portMve
|> Array.map(fun x -> x.Return)
|> Seq.stDev
|> fun vol -> sqrt(12.0) * vol(* output: 
0.04081523203*)
(**
To the 60/40 vol.
*)
port6040
|> Array.map(fun x -> x.Return)
|> Seq.stDev
|> fun vol -> sqrt(12.0)*vol(* output: 
0.09986539511*)
(**
Ok, cumulative returns of the normalized vol returns.
*)
let normalize10pctVol xs =
    let vol = xs |> Array.map(fun x -> x.Return) |> Seq.stDev
    let annualizedVol = vol * sqrt(12.0)
    xs 
    |> Array.map(fun x -> { x with Return = x.Return * (0.1/annualizedVol)})

let portMveCumulativeNormlizedVol = 
    portMve
    |> normalize10pctVol
    |> cumulateReturns

let port6040CumulativeNormlizedVol = 
    port6040
    |> normalize10pctVol 
    |> cumulateReturns


let chartMVENormlizedVol = 
    portMveCumulativeNormlizedVol
    |> Array.map(fun x -> x.Date, x.Return)
    |> Chart.Line
    |> Chart.withTraceName "MVE"

let chart6040NormlizedVol = 
    port6040CumulativeNormlizedVol
    |> Array.map(fun x -> x.Date, x.Return)
    |> Chart.Line
    |> Chart.withTraceName "60/40"

let chartCombinedNormlizedVol =
    [| chartMVENormlizedVol; chart6040NormlizedVol |]
    |> Chart.Combine
chartCombinedNormlizedVol |> Chart.Show
(**
## Key points to keep in mind.
The mean-variance efficient portfolio will always look best in the sample period 
in which you estimated the weights.
This is because we found it by literally looking for the portfolio 
with the highest sharpe ratio in that sample.

A more meaningful comparison would be to estimate mean-variance efficient weights based
on past data and see how those weights perform in future data. 
For instance, estimate weights 2000-2010, and use those weights to determine
the portfolio that you're going to hold in 2011.
Finally, compare it to 60/40 in 2011.
That is an "out of sample" test because your test period (2011) 
is different from the period when the weights were estimated (2000-2010).
Then repeat, always using data *before* the holding period as your training period
to estimate the weights for the test holding period. 

It is also important to remember that 10-20 years is not long enough 
to get a good estimate of a portfolio's expected return. 

One way to see this is to compare equity returns 2000-2010 and 2010-2020.
*)
ff3
|> Seq.filter(fun x -> 
    x.Date >= DateTime(2000,1,1) &&
    x.Date <= DateTime(2009,12,31))
|> Seq.averageBy(fun x ->
    12.0*x.MktRf)(* output: 
-0.01768*)
ff3
|> Seq.filter(fun x -> 
    x.Date >= DateTime(2010,1,1) &&
    x.Date <= DateTime(2019,12,31))
|> Seq.averageBy(fun x ->
    12.0*x.MktRf)(* output: 
0.13099*)
(**
Neither of those 10-year periods is a good estimate of expected market returns.
Thus it does not make sense to try forming a mean-variance efficient portfolio
using the trailing 10-year period for estimating forward-looking returns.

If we look at US returns 1900-2012, the data indicates that equity excess returns
were about 5.5%, and bond excess returns were about 1%. 
Covariances over shorter periods are more reasonable,
so we can use the recent sample to estimate covariances and the long sample for means.
*)
let symStockBond = [|"VTI";"BND"|]
let covStockBond =
    symStockBond
    |> Array.map(fun x ->
        symStockBond
        |> Array.map(fun y -> getCov x y stockData))
    |> matrix
let meansStockBond = Vector.ofArray [| 0.055/12.0; 0.01/12.0|]

let wStockBond =
    let w' = Algebra.LinearAlgebra.SolveLinearSystem covStockBond meansStockBond
    w' |> Vector.map(fun x -> x /  Vector.sum w')

wStockBond(* output: 
vector [|0.2480068026; 0.7519931974|]*)
let stockBondSharpeAndSD (weights:float Vector) =
    let sbVar = weights.Transpose * covStockBond * weights
    let sbStDev = sqrt(12.0)*sqrt(sbVar)
    let sbMean = 12.0 * (weights.Transpose * meansStockBond)
    sbMean/sbStDev, sbStDev

stockBondSharpeAndSD wStockBond(* output: 
(0.4389047053, 0.04821161829)*)
stockBondSharpeAndSD (Vector.ofArray [|0.6;0.4|])(* output: 
(0.3947136197, 0.09373884799)*)

