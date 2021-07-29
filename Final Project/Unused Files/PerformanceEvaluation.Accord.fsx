(**
# Performance evaluation

We're going to evaluate portfolio performance. The common way to do this is to estimate a portfolio's return adjusted for risk using a factor model with tradeable risk factors. 

What's a risk factor? These risk factors are portfolios and the idea is that the expected excess return on these risk factors is compensation to investors for bearing the risk inherent in holding those portfolios. For the return variation in these factors to be "risky", it should be something that investors cannot easily diversify. If it was easy to diversify, then investors could put a small bit of the asset in their portfolio and capture the return without affecting portfolio volatility. That would imply being able to increase return without adding risk. Hence the requirement that a factor constitute return variation that is hard to diversify away.

The greater the riskiness of the factor, the greater the factor's expected return (i.e., the risk-return tradeoff). For example, most people feel that stocks are riskier than bonds and indeed stocks have historically had higher returns than bonds.

The risk adjustment involves estimating a portfolio's $\beta$'s on different risk factors. These $\beta$'s constitute the exposure of the portfolio to the risk factor. If the factor return goes up by 1%, then the portfolio's return goes up by $\beta \times 1\%$. 

We can estimate these $\beta$'s by OLS regressions of the portfolio's returns on contemporaneous returns of the risk factors. The slope coefficients on the risk factors are the portfolio's betas on the risk factors. The regression intercept is known as $\alpha$. It represents the average return of the portfolio that is not explained by the portfolio's $\beta$'s on the risk factors. This alpha is the risk-adjusted return. 

Intuitively, $\alpha$ is the average return on a portfolio long the investment you are evaluating and short a portfolio with the same factor risk as that portfolio. If the factors and factor betas accurately measure the portfolio's risk, then the alpha is the portfolio's return that is unrelated to the portfolio's risk. Investors like positive alphas because that implies that the portfolio's return is higher than what investors require for bearing the portfolio's risk.

One thing to keep in mind is that throughout this discussion, we have discussed things from the perspective of arbitrage. That is, like a trader. We have not made any assumptions about utility functions or return distributions. This is the Arbitrage Pricing Theory (APT) of Stephen Ross (1976). He was motivated by the observation that

> "... on theoretical grounds it is difficult to justify either the assumption [in mean-variance anlysis and CAPM] of normality in returns...or of quadratic preferences...and on empirical grounds the conclusions as well as the assumptions of the theory have also come under attack."

The APT way of thinking is less restrictive than economically motivated equilibrium asset pricing models. Which is nice. But it has the cost that it does not tell us as much. With the APT we cannot say precisely what a security's return should be. We can only say that if we go long a portfolio and short the portfolio that replicates its factor exposure, then the alpha shouldn't be *too* big. But if we're thinking like a trader, that's perhaps most of what we care about anyway.


*)
#r "nuget: FSharp.Stats, 0.4.1"
#r "nuget: FSharp.Data"

#load "../common.fsx"

open System
open FSharp.Data
open Common

open FSharp.Stats

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
(**
We get the Fama-French 3-Factor asset pricing model data.
*)
let ff3 = French.getFF3 Frequency.Monthly
(**
Let's get our factor data.
*)
let myFactorPorts = CsvProvider<"myExcessReturnPortfolios.csv",
                                ResolutionFolder = __SOURCE_DIRECTORY__>.GetSample()
(**
Let's start with our long-short portfolio.
*)
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
        
(**
For regression, it is helpful to have the portfolio
return data merged into our factor model data.
*)
type RegData =
    // The ML.NET OLS trainer requires 32bit "single" floats
    { Date : DateTime
      Portfolio : float
      MktRf : float 
      Hml : float 
      Smb : float }

// ff3 indexed by month
// We're not doing date arithmetic, so I'll just
// use DateTime on the 1st of the month to represent a month
let ff3ByMonth = 
    ff3
    |> Array.map(fun x -> DateTime(x.Date.Year, x.Date.Month,1), x)
    |> Map

let longShortRegData =
    longShort 
    |> Array.map(fun port ->
        let monthToFind = DateTime(port.YearMonth.Year,port.YearMonth.Month,1)
        match Map.tryFind monthToFind ff3ByMonth with
        | None -> failwith "probably you messed up your days of months"
        | Some ff3 -> 
            { Date = monthToFind
              Portfolio = port.Return // single converts to 32bit
              MktRf = ff3.MktRf 
              Hml = ff3.Hml 
              Smb = ff3.Smb })
(**
[Accord.NET](http://accord-framework.net/) is a .NET (C#/F#/VB.NET) machine learning library. 

*)
#r "nuget: Accord"
#r "nuget: Accord.Statistics"

open Accord
open Accord.Statistics.Models.Regression.Linear
(**
Now we are going to define our machine learning trainer. OLS!

The OLS trainer is documented [here](https://github.com/accord-net/framework/wiki/Regression) with an example in C#. 

We'll use it in a a more F# way
*)
type RegressionOutput =
    { Model : MultipleLinearRegression 
      TValuesWeights : float array
      TValuesIntercept : float }

/// Type alias for x, y regression data 
type XY = (float array) array * float array

let fitModel (data: XY) =
    let x, y = data
    let ols = new OrdinaryLeastSquares(UseIntercept=true)
    let estimate = ols.Learn(x,y)
    let mse = estimate.GetStandardError(x,y)
    let se = estimate.GetStandardErrors(mse, ols.GetInformationMatrix())
    let tvaluesWeights = 
        estimate.Weights
        |> Array.mapi(fun i w -> w / se.[i])
    let tvalueIntercept = estimate.Intercept / (se |> Array.last)
    { Model = estimate
      TValuesWeights = tvaluesWeights
      TValuesIntercept = tvalueIntercept  }

let capmModelData = 
    longShortRegData
    |> Array.map(fun obs -> [|obs.MktRf|], obs.Portfolio)
    |> Array.unzip 

let ff3ModelData = 
    longShortRegData
    |> Array.map(fun obs -> [|obs.MktRf; obs.Hml; obs.Smb |], obs.Portfolio)
    |> Array.unzip

let r2Of (estimatedModel:MultipleLinearRegression) data =
    let x, y = data
    estimatedModel.CoefficientOfDetermination(x,y)
(**
Now we can estimate our models.
*)
let capmEstimate = capmModelData |> fitModel
let ff3Estimate = ff3ModelData |> fitModel
(**
The results can be found in [OLSModelParameters Class](https://docs.microsoft.com/en-us/dotnet/api/microsoft.ml.trainers.olsmodelparameters?view=ml-dotnet).
CAPM results.
*)
capmEstimate.Model(* output: 
val it : MultipleLinearRegression =
  y(x0) = -0.4822107727535372*x0 + 0.004714465103787669
    {Coefficients = [|-0.4822107728|];
     HasIntercept = false;
     Inputs = 1;
     Intercept = 0.004714465104;
     NumberOfInputs = 1;
     NumberOfOutputs = 1;
     NumberOfParameters = 2;
     Weights = [|-0.4822107728|];}*)
capmEstimate.TValuesIntercept(* output: 
val it : float = 2.604590557*)
r2Of capmEstimate.Model capmModelData  (* output: 
val it : float = 0.3755846114*)
(**
Fama-French 3-Factor model results
*)
ff3Estimate.Model(* output: 
val it : MultipleLinearRegression =
  y(x0, x1, x2) = -0.46761365532881344*x0 + 0.38617015533621*x1 + -0.06578738444782757*x2 + 0.004456146290107995
    {Coefficients = [|-0.4676136553; 0.3861701553; -0.06578738445|];
     HasIntercept = false;
     Inputs = 3;
     Intercept = 0.00445614629;
     NumberOfInputs = 3;
     NumberOfOutputs = 1;
     NumberOfParameters = 4;
     Weights = [|-0.4676136553; 0.3861701553; -0.06578738445|];}*)
ff3Estimate.TValuesIntercept(* output: 
val it : float = 2.761917451*)
r2Of ff3Estimate.Model ff3ModelData (* output: 
val it : float = 0.5089703632*)
(**
You will probably see that the CAPM $R^2$ is lower than the
Fama-French $R^2$. This means that you can explain more of the
portfolio's returns with the Fama-French model. Or in trader terms,
you can hedge the portfolio better with the multi-factor model.
We also want predicted values so that we can get regression residuals for calculating
the information ratio. 

*)
type Prediction = { Label : float; Score : float}

let makePredictions (estimate:MultipleLinearRegression) (data:XY) =
    let x, y = data
    (estimate.Transform(x), y)
    ||> Array.zip
    |> Array.map(fun (score, label) -> { Score = score; Label = label })

let residuals (xs: Prediction array) = xs |> Array.map(fun x -> x.Label - x.Score)

let capmPredictions = makePredictions capmEstimate.Model capmModelData
let ff3Predictions = makePredictions ff3Estimate.Model ff3ModelData

capmPredictions |> Array.take 3(* output: 
*)
capmPredictions |> residuals |> Array.take 3(* output: 
val it : float [] = [|-0.0133922682; -0.03164776188; 0.0122433079|]*)
let capmResiduals = residuals capmPredictions
let ff3Residuals = residuals ff3Predictions
(**
In general I would write a function to do this. Function makes it a bit
simpler to follow. It's hard for me to read the next few lines and understand
what everything is. Too much going on.
*)
let capmAlpha = 12.0 * capmEstimate.Model.Intercept 
let capmStDevResiduals = sqrt(12.0) * (Seq.stDev capmResiduals)
let capmInformationRatio = capmAlpha / capmStDevResiduals(* output: 
val capmAlpha : float = 0.05657358125
val capmStDevResiduals : float = 0.09837607315
val capmInformationRatio : float = 0.5750746033*)
let ff3Alpha = 12.0 * ff3Estimate.Model.Intercept 
let ff3StDevResiduals = sqrt(12.0) * (Seq.stDev ff3Residuals)
let ff3InformationRatio = ff3Alpha / ff3StDevResiduals(* output: 
val ff3Alpha : float = 0.05347375548
val ff3StDevResiduals : float = 0.08723816801
val ff3InformationRatio : float = 0.612962843*)
// Function version

let informationRatio monthlyAlpha (monthlyResiduals: float array) =
    let annualAlpha = 12.0 * monthlyAlpha
    let annualStDev = sqrt(12.0) * (Seq.stDev monthlyResiduals)
    annualAlpha / annualStDev 

informationRatio capmEstimate.Model.Intercept capmResiduals(* output: 
val informationRatio :
  monthlyAlpha:float -> monthlyResiduals:float array -> float
val it : float = 0.5750746033*)
informationRatio ff3Estimate.Model.Intercept ff3Residuals(* output: 
val it : float = 0.612962843*)

