(**
This practice quiz emphasizes `Discriminated Unions`. They are useful for times when the data that you're representing has multiple mutually exclusive cases. 

Here is some good background reading for before you do these questions, particularly the F# for fun and profit link.

- Discriminated Union types
    - The F# language reference for [discriminated unions](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions)
    - If you want more a more in depth discussion, see F# for fun and profit's section on [discriminated unions](https://fsharpforfunandprofit.com/posts/discriminated-unions/)
    - The tour of F# section on [discriminated unions](https://docs.microsoft.com/en-us/dotnet/fsharp/tour#record-and-discriminated-union-types)
*)
/// *****************
/// Questions
/// *****************
/// 
(*
Create a discriminated union named `Action` with two cases: Buy and Sell.
  - Create a value named 'bAction' and assign `Buy` to it.
  - Create a value named 'sAction' and assign `Sell` to it.
*)

type Feelings = | Love | Hate 
let aFeelings = Love
let bFeelings = Hate



type Action = Buy | Sell 
let bAction = Buy
let sAction = Sell
(*
Create a single case discriminated union to represent a particular kind of string:
    - Create a discriminated union named Ticker with a single case Ticker of string. 
    - Then wrap the string "ABC" in your Ticker type and assign it to a value named 'aTicker'.
    - Then use pattern matching to unwrap the string in `aTicker` and assign it to a value named `aTickerString`.
*)
type Ticker = 
    | Ticker of string
let aTicker = Ticker "ABC"
let aTickerString =
    match aTicker with
    | Ticker at -> at
// or
let (Ticker aTickerString) = aTicker

(*
Create a single case discriminated union to represent a particular kind of float:
    - Create a discriminated union named Signal with a single case Signal of float. 
    - Then wrap the string float `1.0` in your Signal type and assign it to a value named 'aSignal'.
    - Then use pattern matching to unwrap the float in `aSignal` and assign it to a value named `aSignalFloat`.
*)
type Signal =
    | Signal of decimal
let aSignal = Signal 10.25m
let aSignalDecimal = 
    match aSignal with
    | Signal n -> n

let optionFun (x: int Option) =
    match x with
    | Some x -> Some (x + 1)
    | None -> None
optionFun (Some 6)
optionFun (None)



type Signal =
    | Signal of float
let aSignal = Signal 1.0
let (Signal aSignalFloat) = aSignal
(*
Create a discriminated union called called `Funds` with two cases: MutualFund of string and HedgeFund of string.
    - Create a MutualFund case of the Fund union with the string "Fidelity Magellan". Assign it to a value named "magellan".
    - Create a HedgeFund case of the Fund union with the string "Renaissance Medallion". Assign it to a value named "renaissance".
*)   

let annualizeMonthlyStdDev monthlyStdDev = 
    monthlyStdDev * sqrt(12.0)

let monthlyStdDev = 0.05

type Monthly = | Monthly of float
let wrapped_MonthlyStdDev = Monthly monthlyStdDev

let annualizedMonthlyStdDev_Only (x: Monthly) =
    match x with
    | Monthly n -> n

annualizedMonthlyStdDev_Only wrapped_MonthlyStdDev


type Funds =
    | MutualFund of string
    | HedgeFund of string
let magellan = MutualFund "Fidelity Magellan"
let renaissance = HedgeFund "Renaissance Medalion"
(*
Imagine that analyst recommendations have the form
```fsharp
type AnalystRec = Buy | Sell | Hold
```
You have recommendations from two analysts
```fsharp
let goldmanRec = Buy
let barclaysRec = Sell
```
You want to act on goldman recommendations as follows:
```fsharp
let actionOnGoldman (x: AnalystRec) =
    match x with
    | Buy | Hold -> "I am buying this!"
    | Sell -> "I am selling this!"
```

The problem is that this `actionOnGoldman` function will
work for both `goldmanRec` and `barclaysRec`.
```fsharp
actionOnGoldman goldmanRec // evaluates to "I am buying this!"
actionOnGoldman barclaysRec // evaluates to "I am selling this!"
```

- Create a single case union called `GoldmanRec` where the single case
is GoldmanRec of AnalystRec. 
- Create a modified `actionOnGoldman` function called `actionOnGoldmanOnly` so that it will only work on recommendations with the type `GoldmanRec`.

If `wrappedGoldmanRec` is buy `GoldmanRec`, the result should be
```
actionOnGoldmanOnly wrappedGoldmanRec // evaluates to "I am buying this!"
actionOnGoldmanOnly barclaysRec // compiler error.
```
*)
type AnalystRec = Buy | Sell | Hold
let goldmanRec = Buy
let barclaysRec = Sell
let actionOnGoldman (x: AnalystRec) =
    match x with
    | Buy | Hold -> "I am buying this!"
    | Sell -> "I am selling this!"
actionOnGoldman goldmanRec // evaluates to "I am buying this!"
actionOnGoldman barclaysRec // evaluates to "I am selling this!"

type GoldmanRec = | GoldmanRec of AnalystRec
let actionOnGoldmanOnly (x: GoldmanRec) =
    match x with
    | GoldmanRec Buy | GoldmanRec Hold -> "I am buying this!"
    | GoldmanRec Sell -> "I am selling this!"

let wrappedGoldmanRec = GoldmanRec Buy
actionOnGoldmanOnly wrappedGoldmanRec
// actionOnGoldmanOnly barclaysRec

// or
let actionOnGoldman2 (GoldmanRec x) = actionOnGoldman x
actionOnGoldman2 wrappedGoldmanRec

(*
Imagine that stock tips have the form
```fsharp
type StockTip = Buy | Sell | Hold
```
You have recommendations from two people
```fsharp
let friendRec = Buy
let professorRec = Sell
```
You want to actions as follows:
```fsharp
let actionOnFriend (x: StockTip) = x
let actionOnProfessor (x: StockTip) =
    match x with
    | Buy -> StockTip.Sell
    | Hold -> StockTip.Sell
    | Sell -> StockTip.Buy
```

- Create a two case union called `FriendOrFoe` where the two cases are Friend of StockTip and Professor of StockTip.
- Create a function called `actionFriendOrFoe` that will properly handle tips from friends and tips from professors.

Show that `friendRec` and `professorRec` wrapped in the `FriendOrFoe` type are handled properly by `actionFriendOrFoe`.
*)
type StockTip = Buy | Sell | Hold
let friendRec = Buy
let professorRec = Sell
let actionOnFriend (x: StockTip) = x
let actionOnProfessor (x: StockTip) =
    match x with
    | Buy -> StockTip.Sell
    | Hold -> StockTip.Sell
    | Sell -> StockTip.Buy

type FriendOrFoe = 
    | Friend of StockTip
    | Professor of StockTip

let actionFriendOrFoe (x: FriendOrFoe) =
    match x with 
    | Friend Buy | Professor Sell -> "Buy"
    | Friend Hold -> "Hold"
    | Friend Sell | Professor Buy | Professor Hold -> "Sell"

actionFriendOrFoe (Friend friendRec)
actionFriendOrFoe (Professor professorRec)

// 0r 
let actionFriendOrFoe1 (x: FriendOrFoe) =
    match x with 
    | Friend tip -> actionOnFriend tip
    | Professor tip -> actionOnProfessor tip

actionFriendOrFoe1 (Friend friendRec)
actionFriendOrFoe1 (Professor professorRec)




open System
type ReturnOb = { Symbol : string; Date : DateTime; Return : float }
let returns = 
    [| { Symbol = "AAPL"
         Date = DateTime(2021,1,1)
         Return = -0.02993466474 }
       { Symbol = "AAPL"
         Date = DateTime(2020,1,1)
         Return = 5.0299334383474 }
       { Symbol = "AAPL"
         Date = DateTime(2021,2,1)
         Return = -0.01872509079 }
       { Symbol = "TSLA"
         Date = DateTime(2021,3,1)
         Return = -0.1487757845 }
       { Symbol = "TSLA"
         Date = DateTime(2021,1,1)
         Return = 0.1059620976 } |]
         
returns
|> Array.groupBy(fun x -> x.Symbol, x.Date.Year, x.Date.Month)
|> Array.map(fun((symbol, year_, month_ ), xs) -> 
    let min = xs |> Array.map(fun x -> x.Return) |> Array.min
    { Symbol = symbol 
      Date = xs |> Array.map(fun x -> x.Date) |> Array.max
      Return = min }
    )



/// *****************
/// Answers
/// *****************
/// 
(*
Create a discriminated union named `Action` with two cases: Buy and Sell.
  - Create a value named 'bAction' and assign `Buy` to it.
  - Create a value named 'sAction' and assign `Sell` to it.
*)
type Action = 
    | Buy 
    | Sell
let bAction = Buy
let sAction = Sell
(*
Create a single case discriminated union to represent a particular kind of string:
    - Create a discriminated union named Ticker with a single case Ticker of string. 
    - Then wrap the string "ABC" in your Ticker type and assign it to a value named 'aTicker'.
    - Then use pattern matching to unwrap the string in `aTicker` and assign it to a value named `aTickerString`.
*)
// 

// Discriminated unions like this are usful if you want to make 
// sure that you don't accidentally mix up two strings that represent
// different things.
// A function that takes an input with type `Ticker` will not accept any string,
// it will only accept inputs that have time Ticker.
type Ticker = Ticker of string
let aTicker = Ticker "ABC"
let (Ticker aTickerString) = aTicker
(*
Create a single case discriminated union to represent a particular kind of float:
    - Create a discriminated union named Signal with a single case Signal of float. 
    - Then wrap the string float `1.0` in your Signal type and assign it to a value named 'aSignal'.
    - Then use pattern matching to unwrap the float in `aSignal` and assign it to a value named `aSignalFloat`.
*)
//
type Signal = Signal of float
let aSignal = Signal 1.2
let (Signal aSignalFloat) = aSignal
(*
Create a discriminated union called called `Funds` with two cases: MutualFund of string and HedgeFund of string.
    - Create a MutualFund case of the Fund union with the string "Fidelity Magellan". Assign it to a value named "magellan".
    - Create a HedgeFund case of the Fund union with the string "Renaissance Medallion". Assign it to a value named "renaissance".
*)       
type Funds =
    | MutualFund of string
    | HedgeFund of string

let magellan = MutualFund "Fidelity Magellan"
let renaissance = HedgeFund "Renaissance Medallion"
(*
Imagine that analyst recommendations have the form
```fsharp
type AnalystRec = Buy | Sell | Hold
```
You have recommendations from two analysts
```fsharp
let goldmanRec = Buy
let barclaysRec = Sell
```
You want to act on goldman recommendations as follows:
```fsharp
let actionOnGoldman (x: AnalystRec) =
    match x with
    | Buy | Hold -> "I am buying this!"
    | Sell -> "I am selling this!"
```

The problem is that this `actionOnGoldman` function will
work for both `goldmanRec` and `barclaysRec`.
```fsharp
actionOnGoldman goldmanRec // evaluates to "I am buying this!"
actionOnGoldman barclaysRec // evaluates to "I am selling this!"
```

- Create a single case union called `GoldmanRec` where the single case
is GoldmanRec of AnalystRec. 
- Create a modified `actionOnGoldman` function called `actionOnGoldmanOnly` so that it will only work on recommendations with the type `GoldmanRec`.

If `wrappedGoldmanRec` is buy `GoldmanRec`, the result should be
```
actionOnGoldmanOnly wrappedGoldmanRec // evaluates to "I am buying this!"
actionOnGoldmanOnly barclaysRec // compiler error.
```
*)
type AnalystRec = Buy | Sell | Hold
type GoldmanRec = GoldmanRec of AnalystRec
let goldmanRec = Buy
let barclaysRec = Sell
let actionOnGoldman (x: AnalystRec) =
    match x with
    | Buy | Hold -> "I am buying this!"
    | Sell -> "I am selling this!"
actionOnGoldman goldmanRec // what we want.
actionOnGoldman barclaysRec // oops.
// constructing it from scratch.
let wrappedGoldmanRec = GoldmanRec Buy
// or, wrapping our previously created value
let wrappedGoldmanRec2 = GoldmanRec goldmanRec
wrappedGoldmanRec = wrappedGoldmanRec2 // true
// constructing it from scratch
let actionOnGoldmanOnly (x: GoldmanRec) =
    match x with
    | GoldmanRec Buy | GoldmanRec Hold -> "I am buying this!"
    | GoldmanRec Sell -> "I am selling this!"
// or, unwrapping the GoldmanRec with pattern matching
// in the input definition:
let actionOnGoldmanOnly2 (GoldmanRec x) =
    // Since we unwrapped the goldman recommendation,
    // now it is just the inner analyst recommendation.
    // We can leave off the GoldmanRec that was wrapping the
    // recomendation.
    match x with
    | Buy | Hold -> "I am buying this!"
    | Sell -> "I am selling this!"
// or, since you see above that once we unwrap the goldman rec,
// it is the same as our orignal function.
let actionOnGoldmanOnly3 (GoldmanRec x) = actionOnGoldman x

// check
actionOnGoldmanOnly wrappedGoldmanRec
actionOnGoldmanOnly2 wrappedGoldmanRec
actionOnGoldmanOnly3 wrappedGoldmanRec
// These would all give compiler errors. 
// Uncomment them (delete the // at the start) to test them yourself.
//actionOnGoldmanOnly barclaysRec
//actionOnGoldmanOnly2 barclaysRec
//actionOnGoldmanOnly3 barclaysRec
(*
Imagine that stock tips have the form
```fsharp
type StockTip = Buy | Sell | Hold
```
You have recommendations from two people
```fsharp
let friendRec = Buy
let professorRec = Sell
```
You want to actions as follows:
```fsharp
let actionOnFriend (x: StockTip) = x
let actionOnProfessor (x: StockTip) =
    match x with
    | Buy -> StockTip.Sell
    | Hold -> StockTip.Sell
    | Sell -> StockTip.Buy
```

- Create a two case union called `FriendOrFoe` where the two cases are Friend of StockTip and Professor of StockTip.
- Create a function called `actionFriendOrFoe` that will properly handle tips from friends and tips from professors.

Show that `friendRec` and `professorRec` wrapped in the `FriendOrFoe` type are handled properly by `actionFriendOrFoe`.
*)
type StockTip = Buy | Sell | Hold
let friendRec = Buy
let professorRec = Sell
let actionOnFriend (x: StockTip) = x
let actionOnProfessor (x: StockTip) =
    match x with
    | Buy -> StockTip.Sell
    | Hold -> StockTip.Sell
    | Sell -> StockTip.Buy
// or, since we're doing the same thing with a professor's
// Buy or Hold recommendation, this could also be written
let actionOnProfessor2 (x: StockTip) =
    match x with
    | Buy | Hold -> StockTip.Sell
    | Sell -> StockTip.Buy
type FriendOrFoe = 
    | Friend of StockTip
    | Professor of StockTip
let wrappedFriendRec = Friend friendRec
let wrappedProfessorRec = Professor professorRec

let actionOnFriendOrFoe (x: FriendOrFoe) =
    match x with
    | Friend tip -> actionOnFriend tip
    | Professor tip -> actionOnProfessor tip 

actionOnFriendOrFoe wrappedFriendRec // evaluates to Buy
actionOnFriendOrFoe wrappedProfessorRec // evaluates to Buy
actionOnFriendOrFoe (Professor Hold) // evaluates to Sell
