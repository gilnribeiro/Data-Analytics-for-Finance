(**
# Performance evaluation
We're going to evaluate portfolio performance. The common way to do this is to estimate a portfolio's return adjusted for risk using a factor model with tradeable risk factors. 
*)
#r "nuget: FSharp.Stats, 0.4.1"
#r "nuget: FSharp.Data"
#r "nuget: Plotly.NET, 2.0.0-beta9"
#r "nuget:NodaTime"

#load "../common.fsx"

open System
open FSharp.Data
open NodaTime
open Plotly.NET
open Common

open FSharp.Stats

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

// Get the Fama-French 3-Factor asset pricing model data.
let ff3 = French.getFF3 Frequency.Monthly

// Get our factor data.

let myFactorPorts = CsvProvider<"myExcessReturnPortfolios.csv",
                                ResolutionFolder = __SOURCE_DIRECTORY__>.GetSample()

#r "nuget:Microsoft.ML,1.5"
#r "nuget:Microsoft.ML.MKL.Components,1.5"

open Microsoft.ML
open Microsoft.ML.Data

// Long-short portfolio.

let long = myFactorPorts.Rows |> Seq.filter(fun row -> row.PortfolioName = "Mine" && row.Index = Some 3)
let short = myFactorPorts.Rows |> Seq.filter(fun row -> row.PortfolioName = "Mine" && row.Index = Some 1)

type Return = { YearMonth : DateTime; Return : float }
let longShort =
    // this is joining long to short by YearMonth:DateTime
    let shortMap = short |> Seq.map(fun row -> row.YearMonth, row) |> Map
    long
    |> Seq.map(fun longObs -> 
        match Map.tryFind longObs.YearMonth shortMap with
        | None -> failwith "probably your date variables are not aligned"
        | Some shortObs -> { YearMonth = longObs.YearMonth; Return = longObs.Ret - shortObs.Ret })
    |> Seq.toArray    


// PLOTS

type PortReturn = { PortfolioId : string; YearMonth : DateTime; Return : float }
let cumulateReturn (xs: PortReturn array) =
    let mapper (priorRet:float) (thisObservation:PortReturn) =
        let asOfNow = priorRet*(1.0 + thisObservation.Return)
        { thisObservation with Return = asOfNow}, asOfNow
    // remember to make sure that your sort order is correct.
    let sorted = xs |> Array.sortBy(fun x -> x.YearMonth)
    (1.0, sorted) 
    ||> Array.mapFold mapper 
    |> fst    

// Auxiliary long portfolio as an Array
let longArray = 
    long
    |> Seq.map(fun longObs -> 
        { PortfolioId = "Long"; YearMonth = longObs.YearMonth; Return = longObs.Ret})
    |> Seq.toArray 

let longShortArray = 
    longShort
    |> Array.map(fun longObs -> 
        { PortfolioId = "Long-Short"; YearMonth = longObs.YearMonth; Return = longObs.Return})

let cumulativeLong = longArray |> cumulateReturn
let cumulativeLS = longShortArray |> cumulateReturn
(**
Plotly.NET doesn't know about YearMonth, so I we have to convert to DateTime before plotting.
*)
let cumulativeChart cumulative =
    cumulative
    |> Array.map(fun x -> DateTime(x.YearMonth.Year,x.YearMonth.Month,1), x.Return)
    |> Chart.Line

cumulativeChart cumulativeLong
|> Chart.withTitle "Growth 1 Euro (Not Standardized)"
|> Chart.Show

cumulativeChart cumulativeLS
|> Chart.withTitle "Growth 1 Euro (Not Standardized)"
|> Chart.Show
(**
Create one normalized to have 10\% annualized volatility
for the entire period. This isn't dynamic rebalancing. We're
just making the whole time-series have 10% vol.
*)
let normalizeToTenPct portfolio =
    let AnnualizedVol = sqrt(12.0) * (portfolio |> Seq.stDevBy(fun x -> x.Return))
    portfolio 
    |> Array.map(fun x -> { x with Return = (0.1/AnnualizedVol) * x.Return })
(**
Check to make sure it's 10\% vol. 
*)
sqrt(12.0) * (normalizeToTenPct longShortArray |> Seq.stDevBy(fun x -> x.Return)) 

// Plot
let NormalizedPlot portfolio =
    normalizeToTenPct portfolio
    |> cumulateReturn
    |> Array.map(fun x -> DateTime(x.YearMonth.Year,x.YearMonth.Month,1), x.Return)
    |> Chart.Line 
    |> Chart.withTitle "Growth of 1 Euro"

NormalizedPlot longArray |> Chart.Show
NormalizedPlot longShortArray |> Chart.Show

// Get joint Plots

let vwMktRf =
    let portfolioMonths = longShort |> Array.map(fun x -> x.YearMonth)
    let minYm = portfolioMonths |> Array.min
    let maxYm = portfolioMonths |> Array.max
    
    ff3
    |> Array.map(fun x -> 
        { PortfolioId = "Mkt-Rf"
          YearMonth = DateTime(x.Date.Year, x.Date.Month,1)
          Return = x.MktRf })
    |> Array.filter(fun x -> 
        x.YearMonth >= minYm &&
        x.YearMonth <= maxYm)

let combinedChart =
    Array.concat [longArray; longShortArray; vwMktRf]
    |> Array.groupBy(fun x -> x.PortfolioId)
    |> Array.map(fun (portId, xs) ->
        xs
        |> normalizeToTenPct
        |> cumulateReturn
        |> cumulativeChart
        |> Chart.withSize(1200.,450.)
        |> Chart.withTraceName (portId.ToString()+" Normalized"))
    |> Chart.Combine

let combinedChartNotNormalized =
    Array.concat [longArray; longShortArray; vwMktRf]
    |> Array.groupBy(fun x -> x.PortfolioId)
    |> Array.map(fun (portId, xs) ->
        xs
        |> cumulateReturn
        |> cumulativeChart
        |> Chart.withTraceName (portId.ToString()))
    |> Chart.Combine 

let stackedChart = 
    [ combinedChartNotNormalized
      combinedChart ]
    |> Chart.Stack(2,0.05)
    |> Chart.withTitle "Growth of 1 Euro (Clean and Normalized to 10% Volatility)"
    |> Chart.Show
    
(**
For regression, it is helpful to have the portfolio
return data merged into our factor model data.
*)
type RegData =
    // The ML.NET OLS trainer requires 32bit "single" floats
    { Date : DateTime
      Portfolio : single
      MktRf : single 
      Hml : single 
      Smb : single }

// ff3 indexed by month
// We're not doing date arithmetic, so we can just
// use DateTime on the 1st of the month to represent a month
let ff3ByMonth = 
    ff3
    |> Array.map(fun x -> DateTime(x.Date.Year, x.Date.Month,1), x)
    |> Map

// Get Long Reg Data
let longRegData = 
    long |> Seq.toArray
    |> Array.map(fun port ->
        let monthToFind = DateTime(port.YearMonth.Year,port.YearMonth.Month,1)
        match Map.tryFind monthToFind ff3ByMonth with
        | None -> failwith "probably you messed up your days of months"
        | Some ff3 -> 
            { Date = monthToFind
              Portfolio = single port.Ret // single converts to 32bit
              MktRf = single ff3.MktRf 
              Hml = single ff3.Hml 
              Smb = single ff3.Smb })

// Get Long-Short Reg Data
let longShortRegData =
    longShort 
    |> Array.map(fun port ->
        let monthToFind = DateTime(port.YearMonth.Year,port.YearMonth.Month,1)
        match Map.tryFind monthToFind ff3ByMonth with
        | None -> failwith "probably you messed up your days of months"
        | Some ff3 -> 
            { Date = monthToFind
              Portfolio = single port.Return // single converts to 32bit
              MktRf = single ff3.MktRf 
              Hml = single ff3.Hml 
              Smb = single ff3.Smb })

longShortRegData |> Seq.take 3 |> Seq.map(fun x -> printf $"{x}")
longRegData |> Seq.take 3 |> Seq.map(fun x -> printf $"{x}")

longRegData |> Array.length
longShortRegData |> Array.length

(** 
Split the Full period into Bottom and Top Half
*)

type filteredPortfolio =
    { PortfolioId : string
      portfolioData: RegData array}

let splitTimeSort name portfolio =
    portfolio
    |> Array.sortBy(fun x -> x.Date)
    |> Array.splitInto 2
    |> Array.mapi(fun i portData -> 
        { PortfolioId = name + " " + string(i)
          portfolioData = portData})

let bottomHalfLong = 
    splitTimeSort "Long" longRegData
    |> Array.filter(fun x -> x.PortfolioId = "Long 0")
    |> Array.map(fun x -> x.portfolioData)
    |> Array.reduce Array.append

let topHalfLong = 
    splitTimeSort "Long" longRegData
    |> Array.filter(fun x -> x.PortfolioId = "Long 1")
    |> Array.map(fun x -> x.portfolioData)
    |> Array.reduce Array.append

let bottomHalfLS = 
    splitTimeSort "Long Short" longShortRegData
    |> Array.filter(fun x -> x.PortfolioId = "Long Short 0")
    |> Array.map(fun x -> x.portfolioData)
    |> Array.reduce Array.append

let topHalfLS = 
    splitTimeSort "Long Short" longShortRegData
    |> Array.filter(fun x -> x.PortfolioId = "Long Short 1")
    |> Array.map(fun x -> x.portfolioData)
    |> Array.reduce Array.append

longRegData |> Array.length
bottomHalfLS |> Array.length
topHalfLong |> Array.length


// Define a ML.Net 
let ctx = new MLContext()
let mlOLSfunction portfolio =
    
    // Now we can use the context to transform the data into ML.NET's format.
    let portfolioMlData = ctx.Data.LoadFromEnumerable<RegData>(portfolio)
    // Define OLS Trainer
    let trainer = ctx.Regression.Trainers.Ols()
    (**
    Now we define the models to estimate.
    - `Label` is the variable that we are trying to predict or explain with our model.
    - `Features` are the variables that we are using to predict the label column.
    *)
    let capmModel = 
        EstimatorChain()
            .Append(ctx.Transforms.CopyColumns("Label","Portfolio"))
            .Append(ctx.Transforms.Concatenate("Features",[|"MktRf"|])) 
            .Append(trainer)   

    let ff3Model =
        EstimatorChain()
            .Append(ctx.Transforms.CopyColumns("Label","Portfolio"))
            .Append(ctx.Transforms.Concatenate("Features",[|"MktRf";"Hml";"Smb"|]))
            .Append(trainer)   
    (**
    Estimate models.
    *)
    let capmEstimate = portfolioMlData |> capmModel.Fit
    let ff3Estimate = portfolioMlData |> ff3Model.Fit

    // Return Estimates
    capmEstimate, ff3Estimate

// FULL TIME PERIOD
let capmEstimateLS, ff3EstimateLS = mlOLSfunction longShortRegData
let capmEstimateLong, ff3EstimateLong = mlOLSfunction longRegData

//CAPM results - Long Short
capmEstimateLS.LastTransformer.Model
// Fama-French 3-Factor model results - Long Short
ff3EstimateLS.LastTransformer.Model

//CAPM results - Long
capmEstimateLong.LastTransformer.Model
// Fama-French 3-Factor model results - Lon<g
ff3EstimateLong.LastTransformer.Model

// TOP HALF
let capmEstimateLSTop, ff3EstimateLSTop = mlOLSfunction topHalfLS
let capmEstimateLongTop, ff3EstimateLongTop = mlOLSfunction topHalfLong

//CAPM results - Long Short
capmEstimateLSTop.LastTransformer.Model
// Fama-French 3-Factor model results - Long Short
ff3EstimateLSTop.LastTransformer.Model

//CAPM results - Long
capmEstimateLongTop.LastTransformer.Model
// Fama-French 3-Factor model results - Lon<g
ff3EstimateLongTop.LastTransformer.Model

// BOTTOM HALF
let capmEstimateLSBottom, ff3EstimateLSBottom = mlOLSfunction bottomHalfLS
let capmEstimateLongBottom, ff3EstimateLongBottom = mlOLSfunction bottomHalfLong

//CAPM results - Long Short
capmEstimateLSBottom.LastTransformer.Model
// Fama-French 3-Factor model results - Long Short
ff3EstimateLSBottom.LastTransformer.Model

//CAPM results - Long
capmEstimateLongBottom.LastTransformer.Model
// Fama-French 3-Factor model results - Lon<g
ff3EstimateLongBottom.LastTransformer.Model


[<CLIMutable>]
type Prediction = { Label : single; Score : single}

type PredRes = {Portfolio: string 
                capmPredictions: Prediction array
                ff3Predictions: Prediction array
                capmResiduals: single array
                ff3Residuals: single array}

let makePredictions (estimate:TransformerChain<_>) data =
    ctx.Data.CreateEnumerable<Prediction>(estimate.Transform(data),reuseRowObject=false)
    |> Seq.toArray

let residuals (xs: Prediction array) = xs |> Array.map(fun x -> x.Label - x.Score)

let predictionAndResiduals portfolio (name:string) = 

    let capmEstimate, ff3Estimate = mlOLSfunction portfolio
    let portfolioMlData = ctx.Data.LoadFromEnumerable<RegData>(portfolio)

    let capmPredictions = makePredictions capmEstimate portfolioMlData
    let ff3Predictions = makePredictions ff3Estimate portfolioMlData

    let capmResiduals = residuals capmPredictions
    let ff3Residuals = residuals ff3Predictions

    {Portfolio = name
     capmPredictions = capmPredictions
     ff3Predictions = ff3Predictions
     capmResiduals = capmResiduals
     ff3Residuals = ff3Residuals }

// FULL PERIOD
let predResLS = predictionAndResiduals longShortRegData "Long Short"
let predResLong = predictionAndResiduals longRegData "Long"
// TOP HALF
let predResLSTop = predictionAndResiduals topHalfLS "Long Short Top"
let predResLongTop = predictionAndResiduals topHalfLong "Long Top"
// BOTTOM HALF
let predResLSBottom = predictionAndResiduals bottomHalfLS "Long Short Bottom"
let predResLongBottom = predictionAndResiduals bottomHalfLong "Long Bottom"

// Get Statisticst
type stats = { Portfolio: string
               AnnualizedSharpeRatio: single
               AverageAnnualReturn: single
               AnnualizedStDev: single
               annualAlpha: single 
               annualStDevResiduals: single
               informationRatio: single}

let averageAnnualReturn (portfolio:RegData array) =
    let ret = portfolio |> Array.averageBy(fun x -> x.Portfolio)
    single(12.0) * ret

let annualizedStDev (portfolio:RegData array) =
    let stDeviation = portfolio |> stDevBy(fun x -> x.Portfolio)
    single(12.0) * stDeviation

let statistics monthlyAlpha (residuals : single array) (name : string) (portfolio:RegData array)=
    let StdmonthlyResiduals = Seq.stDev residuals
    let annualAlpha = single 12.0 * monthlyAlpha
    let annualStDevResiduals = sqrt(single 12.0) * (StdmonthlyResiduals)
    let informationRatio = annualAlpha / annualStDevResiduals 
    let avgAnnualReturn = averageAnnualReturn portfolio
    let annualizedSd = annualizedStDev portfolio
    let annualizedSharpeRatio = avgAnnualReturn/annualizedSd

    { Portfolio = name
      AnnualizedSharpeRatio = annualizedSharpeRatio
      AverageAnnualReturn = avgAnnualReturn
      AnnualizedStDev = annualizedSd
      annualAlpha = annualAlpha
      annualStDevResiduals = annualStDevResiduals
      informationRatio = informationRatio}

// FULL PERIOD
let CAPMmonthlyAlphaLS = capmEstimateLS.LastTransformer.Model.Bias
let CAPMmonthlyAlphaLong = capmEstimateLong.LastTransformer.Model.Bias

let ff3monthlyAlphaLS = ff3EstimateLS.LastTransformer.Model.Bias
let ff3monthlyAlphaLong = ff3EstimateLong.LastTransformer.Model.Bias

// CAPM Statistics
statistics CAPMmonthlyAlphaLS predResLS.capmResiduals "Long Short CAPM" longShortRegData
statistics CAPMmonthlyAlphaLong predResLong.capmResiduals "Long CAPM" longRegData

// FF3 Statistics
statistics ff3monthlyAlphaLS predResLS.ff3Residuals "Long Short FF3" longShortRegData
statistics ff3monthlyAlphaLong predResLong.ff3Residuals "Long FF3" longRegData

// T-stats
capmEstimateLS.LastTransformer.Model.TValues
capmEstimateLong.LastTransformer.Model.TValues

ff3EstimateLS.LastTransformer.Model.TValues
ff3EstimateLong.LastTransformer.Model.TValues


// TOP HALF
let CAPMmonthlyAlphaLSTop = capmEstimateLSTop.LastTransformer.Model.Bias
let CAPMmonthlyAlphaLongTop = capmEstimateLongTop.LastTransformer.Model.Bias

let ff3monthlyAlphaLSTop = ff3EstimateLSTop.LastTransformer.Model.Bias
let ff3monthlyAlphaLongTop = ff3EstimateLongTop.LastTransformer.Model.Bias

// CAPM Statistics
statistics CAPMmonthlyAlphaLSTop predResLSTop.capmResiduals "Long Short CAPM TOP" topHalfLS
statistics CAPMmonthlyAlphaLongTop predResLongTop.capmResiduals "Long CAPM TOP" topHalfLong

// FF3 Statistics
statistics ff3monthlyAlphaLSTop predResLSTop.ff3Residuals "Long Short FF3 TOP" topHalfLS
statistics ff3monthlyAlphaLongTop predResLongTop.ff3Residuals "Long FF3 TOP" topHalfLong

// T-stats
capmEstimateLSTop.LastTransformer.Model.TValues
capmEstimateLongTop.LastTransformer.Model.TValues

ff3EstimateLSTop.LastTransformer.Model.TValues
ff3EstimateLongTop.LastTransformer.Model.TValues


// BOTTOM HALF
let CAPMmonthlyAlphaLSBottom = capmEstimateLSBottom.LastTransformer.Model.Bias
let CAPMmonthlyAlphaLongBottom = capmEstimateLongBottom.LastTransformer.Model.Bias

let ff3monthlyAlphaLSBottom = ff3EstimateLSBottom.LastTransformer.Model.Bias
let ff3monthlyAlphaLongBottom = ff3EstimateLongBottom.LastTransformer.Model.Bias

// CAPM Statistics
statistics CAPMmonthlyAlphaLSBottom predResLSBottom.capmResiduals "Long Short CAPM BOTTOM" bottomHalfLS
statistics CAPMmonthlyAlphaLongBottom predResLongBottom.capmResiduals "Long CAPM BOTTOM" bottomHalfLong

// FF3 Statistics
statistics ff3monthlyAlphaLSBottom predResLSBottom.ff3Residuals "Long Short FF3 BOTTOM" bottomHalfLS
statistics ff3monthlyAlphaLongBottom predResLongBottom.ff3Residuals "Long FF3 BOTTOM" bottomHalfLong

// T-stats
capmEstimateLSBottom.LastTransformer.Model.TValues
capmEstimateLongBottom.LastTransformer.Model.TValues

ff3EstimateLSBottom.LastTransformer.Model.TValues
ff3EstimateLongBottom.LastTransformer.Model.TValues