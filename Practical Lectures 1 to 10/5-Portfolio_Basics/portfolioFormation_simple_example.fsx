(*
# Building a strategy

Our objective is to write code that will allow us to do something like

samplePeriod
|> filterInvestmentUniverse
|> constructSignals
|> assignPortfolioWeights
|> getPortfolioReturns

*)

#r "nuget: FSharp.Stats,0.4.0"
open FSharp.Stats

(*
## Workflow for using quantitative signals

How do we decide on the position weights for the different securities? An example workflow:

1. **Define your investment universe.** Your investment universe is the set of securities that you are considering for investment. An investment universe is typically a subset of securities grouped by some common characteristics: small-capitalization stocks, utility stocks, corporate bonds, firms listed on Euronext, etc.

2. **Get your signal for each security.** The signal is some information that you believe can be used to predict returns or risk. It could be a stock tip from a friend, a stock analyst recommendation, something you read on reddit, revenue growth, ...

3. **Define a mapping from signals to portfolio weights.** 

Let's go through this step by step.

*)

(*

## 1. Investment universe: modelling a security

We're going to be forming portfolios consisting of weights on different securities. We need a way to identify those securities. Securities can have different identifiers. Stock exchanges use ticker symbols, but CUSIPs are commonly used by other entities (e.g., ratings agencies, regulators, etc.). Some data providers might also use their own custom identifier. For example, Bloomberg has their own "Bloomberg ticker" and CRSP uses PERMNOs. Thus, if we're trying to identify a security we can use tickers OR CUSIPs OR Bloomberg Tickers OR PERMNOs. How do we model this?

A good type for modelling "OR" relationships is a Discriminated Union. The pipe (`|`) symbol indicates an "or" relationship. 
*)
type SecurityId =
    | Ticker of string
    | Cusip of string
    | Bloomberg of string
    | Permno of int


// Defining examples of these ids.
let tickerExample = Ticker "KO"
let permnoExample = Permno 1001

// Deconstructing using pattern matching.
// Think "match what's on the left side of = to what's on the right side"
// This is whats happening - ticker with ticker and decon... with KO
//      let (Ticker deconTickerExample) = Ticker "KO"

let (Ticker deconTickerExample) = tickerExample
printfn "%s" deconTickerExample

let (Permno deconPermnoExample) = permnoExample
printfn "%i" deconPermnoExample

// Now we can define our investment universe.
let investmentUniverse =
    [| "AAPL"; "KO"; "GOOG";"DIS";"GME"|]
    |> Array.map Ticker

(*
## 2. Signals
*)

let signals =
    [| Ticker "AAPL", 2.0
       Ticker "KO", -1.4
       Ticker "GOOG", 0.4 
       Ticker "DIS", 1.1 |]
    |> Map.ofArray

signals.[Ticker "AAPL"]
Map.find (Ticker "AAPL") signals
Map.tryFind (Ticker "GME") signals
Map.tryFind (Ticker "AAPL") signals

// Write a function to get simbol
let getSignal id = Map.tryFind id signals

getSignal (Ticker "AAPL")
getSignal (Ticker "GOOG")

[| for security in investmentUniverse do
     security |]

[| for security in investmentUniverse do
    security, getSignal security |]

investmentUniverse
|> Array.map getSignal

type SecuritySignal = 
    { SecurityId : SecurityId
      Signal : float }

let getSecuritySignal security =
    match Map.tryFind security signals with
    | None -> None
    | Some signal ->
        let result = { SecurityId = security; Signal = signal }
        Some result


getSecuritySignal (Ticker "GME")
getSecuritySignal (Ticker "GOOG")

// Here we can see that we retured something when there
// was a signal and nothing if there was none.
investmentUniverse
|> Array.map getSecuritySignal
|> Array.iter (printfn "%A")

// If we do choose instead of map, then we will
// end up with only the results when there was something.
investmentUniverse
|> Array.choose getSecuritySignal
|> Array.iter (printfn "%A") // iteri adds an index to iter

let securitySignals = 
    investmentUniverse
    |> Array.choose getSecuritySignal

securitySignals
|> Array.iter (printfn "%A")

(*
## 3. Defining a mapping between signals and portfolio weights.

Now let's look at turning signals into weights.

Often, you only want stocks with signals above or below a given threshold in a portfolio. For instance, if you have a "value" portfolio you might only want stocks with low price to earnings (P/E) ratios in your portfolio. Or maybe you want to go long value stocks and short growth stocks.

A typical procedure is to assign securities to portfolios based on signals, and then weight securities within those sub portfolios.

*)

(*

First, let's represent portfolios Ids.
A first step is to define portfolio IDs. A simple ID is a string name, but often we will do things like create 10 size portfolios infexed from 1 to 10, like ("Size", 1), ("Size", 2), ... ("Size", 10) - i.e. the 10% Biggest stocks in the investment univverse - We can model this as a discriminated union. 

Here, `Indexed` is a tuple where the first element is a string and the second is an integer. I could just say `Indexed of string * int`, but I am going to name them to give them meaning. Though the names are optional when constructing and deconstructing
*)

// Define a Portfolio ID
// We allow for either a name (str) or a name and index (str * int)
type PortfolioId = 
    | Named of string
    | Indexed of name:string * index:int

// Example portfolio IDs
let portolioId1 = Named "Market"
let portfolioId2 = Indexed ("Size",1)
let portfolioId3 = Indexed (index=2,name="Size")    

// Example deconstructing
let (Indexed(deconPortName,deconPortIndex)) = portfolioId3

(*
    WELL DIVIDE SORT OF BETWEEN VALUE VS. GROWTH
Let's assign securities to portolios based on whether their signal is above or below the median.
*)

// Just an example
type ExampleTest1 = 
    { PortfolioId : string
      Signals : float}
{ PortfolioId = "hi"; Signals = 4.0}
// Just an example

// Model for an assigned portfolio
type AssignedPortfolio =
    { PortfolioId : PortfolioId 
      Signals : SecuritySignal array }

// Lets split securitySignals to median
let medianSignal = 
    securitySignals 
    |> Array.map(fun x -> x.Signal)
    |> Array.median

// Lets assing the signals that are above the median we just calculated
let aboveMedian =
    securitySignals
    |> Array.filter(fun x -> x.Signal >= medianSignal)

// Assign the below median (filter is great for this)
let belowMedian =
    securitySignals
    |> Array.filter(fun x -> x.Signal < medianSignal)

// Create  an array of those 2 assigned portfolios
// of type AssignedPortfolio we defined above
let assigned =
    [| { PortfolioId = Named("Above Median")
         Signals = aboveMedian }
       { PortfolioId = Named("Below Median")
         Signals = belowMedian} |]

assigned.[0]
assigned.[1]

// Using GroupBy (with 2 portfolios kinda works, with 10 you'd have to create the 10 different groups)
securitySignals
|> Array.groupBy(fun x -> 
    if x.Signal < medianSignal
    then Named ("Below Median")
    else Named ("Above Median")
    )

// Or create a reusable function to do the same thing
// this does all the above in one step
let assignAboveBelowMedian securitySignals =
    let medianSignal = 
        securitySignals 
        |> Array.map(fun x -> x.Signal)
        |> Array.median

    let aboveMedian =
        securitySignals
        |> Array.filter(fun x -> x.Signal >= medianSignal)

    let belowMedian =
        securitySignals
        |> Array.filter(fun x -> x.Signal < medianSignal)

    [| { PortfolioId = Named("Above Median")
         Signals = aboveMedian }
       { PortfolioId = Named("Below Median")
         Signals = belowMedian} |]


(* 
## Modelling a position
Now we have assigned securities to portfolios based on the trading signal. Now we can form weights. We can think of a portfolio as consisting of positions where positions are symbols and weights.
*)

type Position =
    { SecurityId : SecurityId 
      Weight : float }

// Defining example positions

let koPosition = { SecurityId = Ticker "KO"; Weight = 0.25 }
let permnoPosition = { SecurityId = Permno 1001; Weight = 0.75 }

[|koPosition; permnoPosition|]

(*
## Modelling a portfolio
And once we have multiple positions, then we can group them into a portfolio.
*)

(* And a portfolio can consist of a Portfolio Id and an array of positions*)

type Portfolio = 
    { PortfolioId: PortfolioId
      Positions : Position array }

// An example constructing a portfolio

let portfolioExample1 =
    { PortfolioId = Named "Example 1"
      Positions = [| koPosition; permnoPosition |]}


(*
## Defining portfolio position weights

Once you have a portfolio of securities that have met some signal criteria, it is common to weight those securities using either of two simple weighting schemes: equal weight or value weight.

Equal weight means that every security has the same weight.

Value-weight means that you weight securities proportional to their market value. This means that your portfolio put more weight on more valuable securities. Or it "tilts" toward more valuable securities. This weighting scheme is utilized when you want to make sure that you are not putting too much weight on small illiquid securities that are hard to purchase.
*)

// equal-weight is easy
let weightTestPort = 
    assigned |> Array.find(fun x -> x.PortfolioId = Named("Above Median"))
// or, this would also work
let weightTestPort1 = 
    assigned
    |> Array.filter(fun x -> x.PortfolioId = Named("Above Median"))
    |> Array.head

weightTestPort1.PortfolioId
weightTestPort1.Signals

let nSecurities = weightTestPort.Signals.Length

let testSignal = weightTestPort.Signals.[0]
1.0/(float nSecurities)

// Now I have an array of Positions
let ewTestWeights =
    weightTestPort.Signals
    |> Array.map(fun signal ->
        { SecurityId = signal.SecurityId
          Weight = 1.0 / (float nSecurities) })

// Lets create a function that takes an Assigned Portfolio and gives back a Portfolio
let giveEqualWeights x =
    let n = x.Signals.Length
    let pos =
        x.Signals
        |> Array.map(fun signal ->
            { Position.SecurityId = signal.SecurityId
              Weight = 1.0 / (float nSecurities) })
    { PortfolioId = x.PortfolioId 
      Positions = pos }

giveEqualWeights weightTestPort

assigned
|> Array.map giveEqualWeights

// For value weights, we need the securities' market values
// split into above/below median and for portfolios with those.

let marketCapitalizations =
    [| Ticker "AAPL", 10.0
       Ticker "KO", 4.0
       Ticker "GOOG", 7.0 
       Ticker "DIS", 5.0 |]
    |> Map.ofArray

let mktCaps =
    weightTestPort.Signals
    |> Array.map(fun signal ->
            let mktcap = Map.find signal.SecurityId marketCapitalizations
            signal.SecurityId, mktcap)

let vwTestWeights =
    let totMktCap = mktCaps |> Array.sumBy snd
    mktCaps
    |> Array.map(fun (id, mktCap) ->
        { SecurityId = id 
          Weight = mktCap / totMktCap })

let giveValueWeights x =
    let mktCaps =
        x.Signals
        |> Array.map(fun signal ->
            let mktcap = Map.find signal.SecurityId marketCapitalizations
            signal.SecurityId, mktcap)

    let totMktCap = mktCaps |> Array.sumBy snd

    let pos =
        mktCaps
        |> Array.map(fun (id, mktCap) ->
            { SecurityId = id 
              Weight = mktCap / totMktCap })
    { PortfolioId = x.PortfolioId; Positions = pos }

assigned
|> Array.map giveValueWeights  

// All together now
let strategyWeights =
    investmentUniverse
    |> Array.choose getSecuritySignal
    |> assignAboveBelowMedian
    |> Array.map giveValueWeights


// How do we calculate returns?

// Take these returns:
let returnMap =
    [| Ticker "AAPL", -0.4
       Ticker "KO", -0.1
       Ticker "GOOG", 0.15 
       Ticker "DIS", 0.1 |]
    |> Map.ofArray

// What is the return of the two portfolios?
// Hint: the value-weight assignment code is a good for how to look up the stock returns.