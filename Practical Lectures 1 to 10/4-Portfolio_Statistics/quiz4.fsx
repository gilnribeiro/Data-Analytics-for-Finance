(**
This practice quiz emphasizes `Optional types`, `Match expressions`, and `Map Collections`. These are some features that we will use in building a portfolio.

Here are some references to these topics. Please read the F# language reference links before proceeding with the questions. The other links (F# tour and F# for fun and profit) provide additional background and examples but are not necessary:

- Option types
    - The F# language reference for [options](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/options)
    - The tour of F# section on [options](https://docs.microsoft.com/en-us/dotnet/fsharp/tour#optional-types)
    - If you want more a more in depth discussion, see F# for fun and profit's section on [options](https://fsharpforfunandprofit.com/posts/the-option-type/)
- Pattern matching using match expressions.
    - [F# Language reference](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/match-expressions)
    - [Tour of F#](https://docs.microsoft.com/en-us/dotnet/fsharp/tour#pattern-matching)
    - [F# for fun and profit](https://fsharpforfunandprofit.com/posts/match-expression/)
- Map collections.
    - [F# Wikibook](https://en.wikibooks.org/wiki/F_Sharp_Programming/Sets_and_Maps#Maps)

////////
Questions 
////////
*)
type Test = { Time : int; Ret : float }

let xs = 
    [|{ Time=0; Ret= -2.02993466473707298}
      { Time=1; Ret= -1.01872509078933925}
      { Time=0; Ret= 9.190407204169931392}
      { Time=1; Ret= -6.01626157983812109}
      { Time=4; Ret= -7.0767937252017495}
      { Time=4; Ret= 3.13086546989168735}
      { Time=6; Ret= -8.14877578450996193}|]

let fn xs:Test array =
    let minR = xs |> Array.minBy(fun x -> x.Ret, x.Time)
    xs  |> Array.map(fun x ->
    { Time = minR.Time 
      Ret = minR.Ret })
    
fn xs |> Array.last
(*
//////////////
// Options
//////////////

1. Create a value named `a` and assign `Some 4` to it.

2. Create a value name `b` and assign `None` to it.

3. Create a tuple named `c` and assign `(Some 4, None)` to it.

4. Write a function named d that takes `x: float` as an input and outputs
`Some x` if x < 0 and `None` if x >= 0. Test it by mapping each element of
`[0.0; 1.4; -7.0]` by function d.

5. Consider this array of trading days for a stock and it's price and dividends:
```fsharp
type StockDays = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]                 
``` 
    - create a new array called `stockDaysWithDividends` that is a filtered
      version of `stockDays` that only contains days with dividends. 
    - Then create an array called `stockDaysWithoutDividends` that is a filtered
      version of `stockDays` that only contains days that do not have dividends.

6. Consider the value `let nestedOption = (Some (Some 4))`. Pipe
it to `Option.flatten` so that you are left with `Some 4`.

7. Consider this list `let listOfNestedOptions = [(Some (Some 4)); Some (None); None]`.
Show how to transform it into [Some 4; None; None] by mapping a function to each
element of the list. 
*)
// 1
let a = 4
// 2
let b = None
// 3
let c = (Some 4, None)

//  4. Write a function named d that takes `x: float` as an input and outputs
// `Some x` if x < 0 and `None` if x >= 0. Test it by mapping each element of
// `[0.0; 1.4; -7.0]` by function d.
let d (x:float) = if x < 0.0 then Some x else None
[0.0; 1.4; -7.0]
|> List.map(fun x -> x |> d)

(*
5. Consider this array of trading days for a stock and it's price and dividends:
```fsharp
type StockDays = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]                 
``` 
    - create a new array called `stockDaysWithDividends` that is a filtered
      version of `stockDays` that only contains days with dividends. 
    - Then create an array called `stockDaysWithoutDividends` that is a filtered
      version of `stockDays` that only contains days that do not have dividends.
*)
type StockDays = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]

let stockDaysWithDivideds =
    stockDays
    |> Array.filter(fun day -> 
        // variable names are arbitrary, but it's helpful to use
        // meaningful names like "day" if the record that our
        // function is operating on represents a day.
        // using a variable named day to represent the day record
        day.Dividend.IsSome)

let stockDaysWithoutDividends =
    stockDays
    |> Array.filter(fun day ->
        day.Dividend.IsNone)

(*6. Consider the value `let nestedOption = (Some (Some 4))`. Pipe
it to `Option.flatten` so that you are left with `Some 4`.
*)
let nestedOption = (Some (Some 4))

nestedOption
|> Option.flatten

(*
7. Consider this list `let listOfNestedOptions = [(Some (Some 4)); Some (None); None]`.
Show how to transform it into [Some 4; None; None] by mapping a function to each
element of the list. 
*)

let listOfNestedOptions = [(Some (Some 4)); Some (None); None]
listOfNestedOptions
|> List.map(fun x -> x |> Option.flatten)

// map the function Option.flatten to each element of the list
listOfNestedOptions |> List.map Option.flatten
(*
//////////////
// Match Expressions
//////////////

1. Write a function named `ma` that takes `x: float Option` as an input.
Use a match expression to output the float if x is something and
0.0 if the float is nothing. Provide a test case for both cases to show
that the function works.

2. Write a function named `mb` that takes `x: float` as an input.
Use a match expression to output 1.0 if x is 1.0, 4.0 if x is 2.0,
and x^3.0 if x is anything else. Provide 3 tests for the 3 test cases 
to show that the function works.

3. Write a function named `mc` that takes a tuple pair of ints  `x: int * int`
as an input. Handle these cases in the following order:
    - if the first int is 7, return "a".
    - if the second int is 7, return "b".
    - For everything else, return "c"
Finally, test the function on (7,6), (6,7), (7, 7), and (6,6).
Make sure that you understand how those 4 examples are handled.

4. Consider this array of trading days for a stock and it's price and dividends:
```fsharp
type StockDays2 = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays2 = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]                 
``` 
    - create a new array called `daysWithDividends` that is a filtered
      version of `stockDays` that only contains days with dividends. For
      each day with a dividend, you should return a `(int * decimal)` tuple
      where the int is the day  and the decimal is the dividend. 
      Thus the result is an `(int * decimal) array`.
    - Then create an array called `daysWithoutDividends` that is a filtered
      version of `stockDays` that only contains days that do not have dividends.
      For each day without a dividend, you should return the day as an `int`.
      Thus the result is an `int array`.

*)

(*1. Write a function named `ma` that takes `x: float Option` as an input.
Use a match expression to output the float if x is something and
0.0 if the float is nothing. Provide a test case for both cases to show
that the function works.
*)
let ma (x:float Option) = 
    match x with
    | Some y -> Some y
    | None -> Some 0.0
  
ma (Some 42.0)
ma (None)

(*2. Write a function named `mb` that takes `x: float` as an input.
Use a match expression to output 1.0 if x is 1.0, 4.0 if x is 2.0,
and x^3.0 if x is anything else. Provide 3 tests for the 3 test cases 
to show that the function works.
*)
let mb (x:float) = 
    match x with 
    | 2.0 -> 4.0
    | 1.0 -> 1.0
    | _ -> x**3.0

mb 4.0
mb 2.0
mb 1.0
mb 10.0

(*3. Write a function named `mc` that takes a tuple pair of ints  `x: int * int`
as an input. Handle these cases in the following order:
    - if the first int is 7, return "a".
    - if the second int is 7, return "b".
    - For everything else, return "c"
Finally, test the function on (7,6), (6,7), (7, 7), and (6,6).
Make sure that you understand how those 4 examples are handled.
*)

let mc (x: int * int) = 
    match x with
    | (7,_) -> "a"
    | (_,7) -> "b"
    | _ -> "c"

mc (7, 0)
mc (0, 7)
mc (0, 0)
mc (7, 7)
mc (6, 6)

(*4. Consider this array of trading days for a stock and it's price and dividends:
```fsharp
type StockDays2 = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays2 = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]                 
``` 
    - create a new array called `daysWithDividends` that is a filtered
      version of `stockDays` that only contains days with dividends. For
      each day with a dividend, you should return a `(int * decimal)` tuple
      where the int is the day  and the decimal is the dividend. 
      Thus the result is an `(int * decimal) array`.
    - Then create an array called `daysWithoutDividends` that is a filtered
      version of `stockDays` that only contains days that do not have dividends.
      For each day without a dividend, you should return the day as an `int`.
      Thus the result is an `int array`.
*)
type StockDays2 = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays2 = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]

let daysWithDividends =
    stockDays2
    |> Array.filter(fun x -> x.Dividend.IsSome)   
    |> Array.map(fun day -> 
        match day.Dividend with 
        | None -> failwith "shouldn't happen because I filtered on IsSome"
        | Some div -> day.Day, div)

let stockDaysWithDividends3 = 
    stockDays2
    |> Array.choose(fun day ->
        match day.Dividend with 
        | None -> None
        | Some div -> Some (day.Day, div))

let daysWithoutDividends =
    stockDays2
    |> Array.filter(fun x -> 
        match x.Dividend with 
        | None -> true
        | Some y -> false)
    |> Array.map(fun x -> x.Day)

let daysWithoutDividends3 = 
    stockDays2
    |> Array.choose(fun x -> 
        match x.Dividend with
        | None -> Some x.Day
        | Some div -> None)

let daysWithDividends1 =
    // using filter and then a map
    stockDays2
    |> Array.filter (fun day -> day.Dividend.IsSome)
    |> Array.map(fun day ->
        match day.Dividend with
        | None -> failwith "shouldn't happen because I filtered on IsSome"
        | Some div -> day.Day, div)

let daysWithDividends2 =
    // using choose, this is better. Think of choose
    // as a filter on IsSome and a map combined. Choose applies
    // a function that returns an option. If the
    // option result is Some x then choose returns x.
    // If the result is None then choose filters it out.
    // Notice that we don't have to worry about 
    // the "this shouldn't happen" exception
    // because it literally cannot happen in this version.
    // This is an example of making illegal states unrepresentable.
    stockDays2
    |> Array.choose (fun day -> 
        // our function takes a day as an input and outputs
        // a `(int * decimal) option`. That is,
        // an optional tuple.
        match day.Dividend with 
        | None -> None
        | Some div -> Some (day.Day, div))

let daysWithoutDividends =
    stockDays2
    |> Array.choose(fun day -> 
        match day.Dividend with
        | None -> Some day.Day
        | Some div -> None)
(*

//////////////
// Map Collections
//////////////

1. Create a Map collection named `mapA` 
from the list [("a",1);("b",2)] where the first thing 
in the tuple is the key and the second thing is the value.
    - Use Map.tryFind to retrieve the value for key "a"
    - Use Map.tryFind to retrieve the value for key "c"

2. Create a Map collection named `mapB` 
from the list [(1,"a");(2,"b")] where the first thing 
in the tuple is the key and the second thing is the value.
    - Use Map.tryFind to retrieve the value for key 1
    - Use Map.tryFind to retrieve the value for key 3


3. Use this array
```fsharp
type StockDays3 = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays3 = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]     
```
    - Create a Map collection named `mapC`. The key should be the day field, 
      and the value should be the full `StockDays3` record.
    - Create a Map colleciton named `mapD`. The key should be the full
      `StockDay3` record. The value should be the day field.

4. Consider a the following Map collection:
`let mapp = [("a", 1); ("d",7)] |> Map.ofList`
Write a function named `lookFor` that takes `x: string` as an input and
looks up the value in `mapp`. If it finds Some y, print
"I found y" to standard output where y is the actual integer found. 
If it finds None, print "I did not find x" to standard output
where x is the actual key that was looked up. Test it by looking
up "a","b","c","d"



*)

(*1. Create a Map collection named `mapA` 
from the list [("a",1);("b",2)] where the first thing 
in the tuple is the key and the second thing is the value.
    - Use Map.tryFind to retrieve the value for key "a"
    - Use Map.tryFind to retrieve the value for key "c"
*)

type Q = { Day : int; Price : int }

let arrayOfQ = 

    [| for day = 0 to 5 do 

         { Day = day

           Price = 100 - day } |]

let mapQ = 
    arrayOfQ       
    |> Array.map(fun x -> x.Day, x)
    |> Map.ofArray
mapQ |> Map.tryFind 3

let mapA =
    [("a",1);("b",2)]
    |> Map.ofList
mapA |> Map.tryFind "a"
mapA |> Map.tryFind "c"

let mapA = Map [("a",1);("b",2)]
// or
let mapA2 = [("a",1);("b",2)] |> Map
// or
let mapA3 = [("a",1);("b",2)] |> Map.ofList
(*2. Create a Map collection named `mapB` 
from the list [(1,"a");(2,"b")] where the first thing 
in the tuple is the key and the second thing is the value.
    - Use Map.tryFind to retrieve the value for key 1
    - Use Map.tryFind to retrieve the value for key 3
*)

let mapB = 
    [(1,"a");(2,"b")]
    |> Map.ofList

mapB |> Map.tryFind 1
mapB |> Map.tryFind 3

(*3. Use this array
```fsharp
type StockDays3 = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays3 = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]     
```
    - Create a Map collection named `mapC`. The key should be the day field, 
      and the value should be the full `StockDays3` record.
    - Create a Map colleciton named `mapD`. The key should be the full
      `StockDay3` record. The value should be the day field.
*)
type StockDays3 = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays3 = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]

let mapC = 
    stockDays3
    |> Array.map(fun x -> x.Day, x)
    |> Map.ofArray

mapC.TryFind 4
mapC.TryFind 32

let mapD = 
    stockDays3
    |> Array.map(fun x -> x, x.Day)
    |> Map.ofArray
(*4. Consider a the following Map collection:
`let mapp = [("a", 1); ("d",7)] |> Map.ofList`
Write a function named `lookFor` that takes `x: string` as an input and
looks up the value in `mapp`. If it finds Some y, print
"I found y" to standard output where y is the actual integer found. 
If it finds None, print "I did not find x" to standard output
where x is the actual key that was looked up. Test it by looking
up "a","b","c","d"
*)
let mapp = [("a", 1); ("d",7)] |> Map.ofList

let mapp = [("a", 1); ("d",7)] |> Map.ofList

// let lookFor x =
//     match Map.tryFind x mapp with
//     | Some y -> printfn $"I found {y}"
//     | None -> printfn $"I did not find {x}" 

let lookFor (x:string) =
    match Map.tryFind x mapp with
    | Some y -> printfn $"I did find '{x}' with {y}"
    | None -> printfn $"I did not find {x}"

for letter in ["a";"b";"c";"d"] do
    lookFor letter

["a";"b";"c";"d"] |> List.iter lookFor
(*

/////////
// Joins
/////////

For the following questions use this data:
```fsharp
type StockPriceOb =
    { Stock : string 
      Time : int
      Price : int }
type TwoStocksPriceOb =
    { Time : int 
      PriceA : int option 
      PriceB : int option }
let stockA = 
    [{ Stock = "A"; Time = 1; Price = 5}
     { Stock = "A"; Time = 2; Price = 6}]
let stockB =     
    [{ Stock = "B"; Time = 2; Price = 5}
     { Stock = "B"; Time = 3; Price = 4}]
Hint: remember that Map collections are useful for lookups.

1. Create a `TwoStocksPriceOb list` named `tslA` that has prices for
every observation of stockA. If there is a price for stockB
at the same time as stockA, then include the stockB price. Otherwise,
the stockB price should be None.

2. Create a `TwoStocksPriceOb list` named `tslB` that has prices for
every observation of stockB. If there is a price for stockA
at the same time as stockB, then include the stock A price. Otherwise,
the stockA price should be None.

3. Create a `TwoStocksPriceOb list` named `tslC` that only includes times
when there is a price for both stockA and stockB. The prices for stocks
A and B should always be something.

4. Create a `TwoStocksPriceOb list` named `tslD` that includes available
stock prices for stockA and stockB at all possible times. If a price for
one of the stocks is missing for a given time, it should be None.



```
*)

// Hint: remember that Map collections are useful for lookups.
type StockPriceOb =
    { Stock : string 
      Time : int
      Price : int }
type TwoStocksPriceOb =
    { Time : int 
      PriceA : int option 
      PriceB : int option }
let stockA = 
    [{ Stock = "A"; Time = 1; Price = 5}
     { Stock = "A"; Time = 2; Price = 6}]
let stockB =     
    [{ Stock = "B"; Time = 2; Price = 5}
     { Stock = "B"; Time = 3; Price = 4}]
(*1. Create a `TwoStocksPriceOb list` named `tslA` that has prices for
every observation of stockA. If there is a price for stockB
at the same time as stockA, then include the stockB price. Otherwise,
the stockB price should be None.
*)

// order by time
let stocksByTime =
    stockB
    |> List.map(fun x -> x.Time, x.Price)
    |> Map.ofList
let tslA = 
    stockA
    |> List.map(fun x -> 
        let dayB = stocksByTime.TryFind x.Time
        match dayB with
        | Some priceB -> { Time = x.Time ; PriceA = Some x.Price; PriceB = Some priceB}
        | None -> { Time = x.Time ; PriceA = Some x.Price; PriceB = None}
        )

(*2. Create a `TwoStocksPriceOb list` named `tslB` that has prices for
every observation of stockB. If there is a price for stockA
at the same time as stockB, then include the stock A price. Otherwise,
the stockA price should be None.
*)

let stocksByPrice = 
    stockA
    |> List.map(fun x -> x.Price, x)
    |> Map.ofList
// This returns all the prices in stock B and prices in stock A when PB = PA
let tslB =
    stockB
    |> List.map(fun x -> 
        let pA = Map.tryFind x.Price stocksByPrice
        match pA with 
        | Some priceA -> {Time = x.Time ; PriceA = Some priceA.Price; PriceB = Some x.Price}
        | None -> {Time = x.Time ; PriceA = None; PriceB = Some x.Price} 
        )
(*3. Create a `TwoStocksPriceOb list` named `tslC` that only includes times
when there is a price for both stockA and stockB. The prices for stocks
A and B should always be something.
*)
let stockbByTime=
    stockB
    |> List.map(fun x -> x.Time, x)
    |> Map.ofList
let tslC = 
    stockA
    |> List.choose(fun x -> 
        let tB = Map.tryFind x.Time stockbByTime
        match tB with
        | None -> None
        | Some B -> 
            let output = {Time = x.Time ; PriceA = Some x.Price; PriceB = Some B.Price}
            Some output)

// Teacher's Way           
let stockaByTime2 = 
    stockA 
    |> List.map(fun day -> day.Time, day)
    |> Map.ofList
let tslC1 =
    stockB
    |> List.choose(fun dayB ->
        let dayA = Map.tryFind dayB.Time stockaByTime2
        match dayA with
        | None -> None
        | Some da -> 
            let output =
                { Time = dayB.Time
                  PriceA = Some da.Price 
                  PriceB = Some dayB.Price }
            Some output)
(*4. Create a `TwoStocksPriceOb list` named `tslD` that includes available
stock prices for stockA and stockB at all possible times. If a price for
one of the stocks is missing for a given time, it should be None.
*)
// Map stockA
let mapperA = 
    stockA |> List.map(fun x -> x.Time, x) |> Map.ofList
let mapperB = 
    stockB |> List.map(fun x -> x.Time, x) |> Map.ofList

let timeA = stockA |> List.map(fun x -> x.Time) |> set
let timeB = stockB |> List.map(fun x -> x.Time) |> set
let timesAandB = Set.union timeA timeB
let tslD =
    [ for i in timesAandB do
        let st = (Map.tryFind i mapperA, Map.tryFind i mapperB)
        // let stB = Map.tryFind i mapperB
        match st with
        | (Some a, Some b) -> { Time = i; PriceA = Some a.Price ; PriceB = Some b.Price}
        | (Some a, None) -> { Time = i; PriceA = Some a.Price ; PriceB = None}
        | (None, Some b) -> { Time = i; PriceA = None ; PriceB = Some b.Price}
        | (None, None) -> { Time = i; PriceA = None ; PriceB = None}
        ]

// let tslD =
//     stockB
//     |> List.map(fun x -  > 
//         let timeA = Map.tryFind x.Time mapperA
//         match timeA with
//         | Some t when 
//         | Some t -> { Time = x.Time ; PriceA = Some x.Price ; PriceB = Some t.Price}
//         | None -> { Time = x.Time ; PriceA = Some x.Price ; PriceB = None}
//     )
(**
////////
Answers 
////////
*)
(*
//////////////
// Options
//////////////

1. Create a value named `a` and assign `Some 4` to it.

2. Create a value name `b` and assign `None` to it.

3. Create a tuple named `c` and assign `(Some 4, None)` to it.

4. Write a function named d that takes `x: float` as an input and outputs
`Some x` if x < 0 and `None` if x >= 0. Test it by mapping each element of
`[0.0; 1.4; -  7.0]` by function d.

5. Consider this array of trading days for a stock and it's price and dividends:
```fsharp
type StockDays = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]                 
``` 
    - create a new array called `stockDaysWithDividends` that is a filtered
      version of `stockDays` that only contains days with dividends. 
    - Then create an array called `stockDaysWithoutDividends` that is a filtered
      version of `stockDays` that only contains days that do not have dividends.

6. Consider the value `let nestedOption = (Some (Some 4))`. Pipe
it to `Option.flatten` so that you are left with `Some 4`.

7. Consider this list `let listOfNestedOptions = [(Some (Some 4)); Some (None); None]`.
Show how to transform it into [Some 4; None; None] by mapping a function to each
element of the list. 
*)
// 1.
//
let a = Some 4
// 2.
//
let b = None
// 3.
// 
let c = Some 4, None
// 4.
//
let d (x: float) = if x < 0.0 then Some x else None
[0.0; 1.4;-7.0] |> List.map d
// or, we don't actually have to tell it that x is a float
// because type inference can tell that x must be a float
// because the function does `x < 0.0` and 0.0 is a float.
let d2 x = if x < 0.0 then Some x else None
[0.0; 1.4;-7.0] |> List.map d2
    | None -> failwith "Not Implemented"

// 5.
//
type StockDays = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]                 

let stockDaysWithDivideds =
    stockDays
    |> Array.filter(fun day -> 
        // variable names are arbitrary, but it's helpful to use
        // meaningful names like "day" if the record that our
        // function is operating on represents a day.
        // using a variable named day to represent the day record
        day.Dividend.IsSome)
let stockDaysWithoutDividends =
    stockDays
    |> Array.filter(fun x -> 
        // using a variable named x to represent the day record.
        // less clear by looking at this code that x is a day.
        x.Dividend.IsNone)

// 6.
// 
let nestedOption = (Some (Some 4))
nestedOption |> Option.flatten
// this would also work, but doesn't use a pipe
Option.flatten nestedOption

// 7.
//
let listOfNestedOptions = [(Some (Some 4)); Some (None); None]
// map the function Option.flatten to each element of the list
listOfNestedOptions |> List.map Option.flatten
(*
//////////////
// Match Expressions
//////////////

1. Write a function named `ma` that takes `x: float Option` as an input.
Use a match expression to output the float if x is something and
0.0 if the float is nothing. Provide a test case for both cases to show
that the function works.

2. Write a function named `mb` that takes `x: float` as an input.
Use a match expression to output 1.0 if x is 1.0, 4.0 if x is 2.0,
and x^3.0 if x is anything else. Provide 3 tests for the 3 test cases 
to show that the function works.

3. Write a function named `mc` that takes a tuple pair of ints  `x: int * int`
as an input. Handle these cases in the following order:
    - if the first int is 7, return "a".
    - if the second int is 7, return "b".
    - For everything else, return "c"
Finally, test the function on (7,6), (6,7), (7, 7), and (6,6).
Make sure that you understand how those 4 examples are handled.

4. Consider this array of trading days for a stock and it's price and dividends:
```fsharp
type StockDays2 = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays2 = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]                 
``` 
    - create a new array called `daysWithDividends` that is a filtered
      version of `stockDays` that only contains days with dividends. For
      each day with a dividend, you should return a `(int * decimal)` tuple
      where the int is the day  and the decimal is the dividend. 
      Thus the result is an `(int * decimal) array`.
    - Then create an array called `daysWithoutDividends` that is a filtered
      version of `stockDays` that only contains days that do not have dividends.
      For each day without a dividend, you should return the day as an `int`.
      Thus the result is an `int array`.

*)
// 1.
// 
let x = Some "a"

let howdy x =
    match x with
    | None -> "oops"
    | Some x -> x

howdy Some "a"


type Q = { Day : int; Price : int }

let arrayOfQ = 

    [| for day = 0 to 5 do 

         { Day = day

           Price = 100 - day } |]

arr

let ma x = 
    match x with
    | None -> 0.0
    | Some y -> y

ma (Some 7.0) // returns y.0
ma None // returns 0.0
// or, see the x in the (Some x) part of the match expression
// is the float, not the original (x: float Option)
// To see this, hover your cursor over the first two xs. it says x is float Option.
// Then hover over the second two xs. It says x is float. Two different xs!
let ma2 x = 
    match x with
    | None -> 0.0
    | Some x -> x
ma2 (Some 7.0) // returns y.0
ma2 None // returns 0.0

// 2.
//
let mb x = 
    match x with 
    | 1.0 -> 1.0
    | 2.0 -> 4.0
    | x -> x**3.0

mb 1.0 // evaluates to 1.0
mb 2.0 // evaluates to 4.0
mb 7.0 // evaluates to 343.0

// 3.
//
let mc x =
    match x with
    | (7, _) -> "a" // the _ in (7, _) indicates wildcard; it matches anything.
    | (_, 7) -> "b" 
    | _ -> "c" // wild card at the end catches anything remaining.
mc (7,6) // evaluates to "a" because it matches the first case and stops checking.
mc (6,7) // evaluates to "b" because it matches the second case and stops checking.
mc (7,7) // evaluates to "a" because it matches the first case and stops checking.
mc (6,6) // evaluates to "c" because it matches the last wildcard.

// 4.
//
type StockDays2 = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays2 = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]                 

let daysWithDividends1 =
    // using filter and then a map
    stockDays2
    |> Array.filter (fun day -> day.Dividend.IsSome)
    |> Array.map(fun day ->
        match day.Dividend with
        | None -> failwith "shouldn't happen because I filtered on IsSome"
        | Some div -> day.Day, div)

let daysWithDividends2 =
    // using choose, this is better. Think of choose
    // as a filter on IsSome and a map combined. Choose applies
    // a function that returns an option. If the
    // option result is Some x then choose returns x.
    // If the result is None then choose filters it out.
    // Notice that we don't have to worry about 
    // the "this shouldn't happen" exception
    // because it literally cannot happen in this version.
    // This is an example of making illegal states unrepresentable.
    stockDays2
    |> Array.choose (fun day -> 
        // our function takes a day as an input and outputs
        // a `(int * decimal) option`. That is,
        // an optional tuple.
        match day.Dividend with 
        | None -> None
        | Some div -> Some (day.Day, div))

let daysWithoutDividends =
    stockDays2
    |> Array.choose(fun day -> 
        match day.Dividend with
        | None -> Some day.Day
        | Some div -> None)
(*

//////////////
// Map Collections
//////////////

1. Create a Map collection named `mapA` 
from the list [("a",1);("b",2)] where the first thing 
in the tuple is the key and the second thing is the value.
    - Use Map.tryFind to retrieve the value for key "a"
    - Use Map.tryFind to retrieve the value for key "c"

2. Create a Map collection named `mapB` 
from the list [(1,"a");(2,"b")] where the first thing 
in the tuple is the key and the second thing is the value.
    - Use Map.tryFind to retrieve the value for key 1
    - Use Map.tryFind to retrieve the value for key 3


3. Use this array
```fsharp
type StockDays3 = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays3 = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]     
```
    - Create a Map collection named `mapC`. The key should be the day field, 
      and the value should be the full `StockDays3` record.
    - Create a Map colleciton named `mapD`. The key should be the full
      `StockDay3` record. The value should be the day field.

4. Consider a the following Map collection:
`let mapp = [("a", 1); ("d",7)] |> Map.ofList`
Write a function named `lookFor` that takes `x: string` as an input and
looks up the value in `mapp`. If it finds Some y, print
"I found y" to standard output where y is the actual integer found. 
If it finds None, print "I did not find x" to standard output
where x is the actual key that was looked up. Test it by looking
up "a","b","c","d"


*)
// 1.
//
let mapA = Map [("a",1);("b",2)]
// or
let mapA2 = [("a",1);("b",2)] |> Map
// or
let mapA3 = [("a",1);("b",2)] |> Map.ofList

Map.tryFind "a" mapA    // evaluates to Some 1
// or
mapA |> Map.tryFind "a" // evaluates to Some 1
Map.tryFind "c" mapA    // evaluates to None
mapA |> Map.tryFind "c" // evaluates to None 

// 2.
//

let mapB = Map [(1,"a");(2,"b")]
Map.tryFind 1 mapB
Map.tryFind 3 mapB

// 3.
type StockDays3 = { Day : int; Price : decimal; Dividend : decimal Option }
let stockDays3 = 
    [| for day = 0 to 5 do 
        let dividend = if day % 2 = 0 then None else Some 1m
        { Day = day
          Price = 100m + decimal day
          Dividend = dividend } |]     

let mapC =
    stockDays3
    |> Array.map(fun day ->
        // we just want to create a tuple of the (key,value).
        // The key and value can be anything.
        day.Day, day)
    |> Map.ofArray
let mapD =
    stockDays3
    |> Array.map(fun day ->
        // we just want to create a tuple of the (key,value).
        // The key and value can be anything.
        day, day.Day)
    |> Map.ofArray
    
// 4.
//

let mapp = [("a", 1); ("d",7)] |> Map.ofList

let lookFor x =
    match Map.tryFind x mapp with
    | Some y -> printfn $"I found {y}"
    | None -> printfn $"I did not find {x}" 

lookFor "a" // I found 1
lookFor "b" // I did not find b
lookFor "c" // I did not find c
lookFor "d" // I found 7
// or iterate it
// we use iter instead of map
// because the result of iter has type `unit`,
// and iter is for when your function has type `unit`.
// Basically, unit type means the function did something
// (in this case, printed to standard output) but
// it doesn't actually return any output.
// 
// You could use map, but then we get `unit list` which
// isn't really what we want. We just want to iterate
// through the list and print to output.
["a"; "b"; "c"; "d"] |> List.iter lookFor
// or loop it
for letter in ["a"; "b"; "c"; "d"] do
    lookFor letter    
(*

/////////
// Joins
/////////

For the following questions use this data:
```fsharp
type StockPriceOb =
    { Stock : string 
      Time : int
      Price : int }
type TwoStocksPriceOb =
    { Time : int 
      PriceA : int option 
      PriceB : int option }
let stockA = 
    [{ Stock = "A"; Time = 1; Price = 5}
     { Stock = "A"; Time = 2; Price = 6}]
let stockB =     
    [{ Stock = "B"; Time = 2; Price = 5}
     { Stock = "B"; Time = 3; Price = 4}]
Hint: remember that Map collections are useful for lookups.

1. Create a `TwoStocksPriceOb list` named `tslA` that has prices for
every observation of stockA. If there is a price for stockB
at the same time as stockA, then include the stockB price. Otherwise,
the stockB price should be None.

2. Create a `TwoStocksPriceOb list` named `tslB` that has prices for
every observation of stockB. If there is a price for stockA
at the same time as stockB, then include the stock A price. Otherwise,
the stockA price should be None.

3. Create a `TwoStocksPriceOb list` named `tslC` that only includes times
when there is a price for both stockA and stockB. The prices for stocks
A and B should always be something.

4. Create a `TwoStocksPriceOb list` named `tslD` that includes available
stock prices for stockA and stockB at all possible times. If a price for
one of the stocks is missing for a given time, it should be None.



```
*)
type StockPriceOb =
    { Stock : string 
      Time : int
      Price : int }
type TwoStocksPriceOb =
    { Time : int 
      PriceA : int option 
      PriceB : int option }
let stockA = 
    [{ Stock = "A"; Time = 1; Price = 5}
     { Stock = "A"; Time = 2; Price = 6}]
let stockB =     
    [{ Stock = "B"; Time = 2; Price = 5}
     { Stock = "B"; Time = 3; Price = 4}]

// 1.
//      

let stockbByTime = 
    stockB 
    |> List.map(fun day -> day.Time, day)
    |> Map.ofList
let tslA1 =
    stockA
    |> List.map(fun dayA ->
        let dayB = Map.tryFind dayA.Time stockbByTime
        match dayB with
        | None -> 
            { Time = dayA.Time
              PriceA = Some dayA.Price
              PriceB = None}
        | Some db -> 
            { Time = dayA.Time
              PriceA = Some dayA.Price 
              PriceB = Some db.Price })
// or, just a personal preference if you like the loop or List.map
let tslA2 =
    [ for dayA in stockA do 
        let dayB = Map.tryFind dayA.Time stockbByTime
        match dayB with
        | None -> 
            { Time = dayA.Time
              PriceA = Some dayA.Price
              PriceB = None}
        | Some db -> 
            { Time = dayA.Time
              PriceA = Some dayA.Price 
              PriceB = Some db.Price }]
// or, define a function
let tryFindBforA (dayA: StockPriceOb) =
    let dayB = Map.tryFind dayA.Time stockbByTime
    match dayB with
    | None -> 
        { Time = dayA.Time
          PriceA = Some dayA.Price
          PriceB = None}
    | Some db -> 
        { Time = dayA.Time
          PriceA = Some dayA.Price 
          PriceB = Some db.Price }   
// checkit
tryFindBforA stockA.[0]
// do it
let tslA3 = stockA |> List.map tryFindBforA                         

// 2.
//     
let stockaByTime = 
    stockA 
    |> List.map(fun day -> day.Time, day)
    |> Map.ofList
let tslB =
    stockB
    |> List.map(fun dayB ->
        let dayA = Map.tryFind dayB.Time stockaByTime
        match dayA with
        | None -> 
            { Time = dayB.Time
              PriceA = None
              PriceB = Some dayB.Price }
        | Some da -> 
            { Time = dayB.Time
              PriceA = Some da.Price 
              PriceB = Some dayB.Price })

//3.
//
let stockaByTime2 = 
    stockA 
    |> List.map(fun day -> day.Time, day)
    |> Map.ofList
let tslC1 =
    stockB
    |> List.choose(fun dayB ->
        let dayA = Map.tryFind dayB.Time stockaByTime2
        match dayA with
        | None -> None
        | Some da -> 
            let output =
                { Time = dayB.Time
                  PriceA = Some da.Price 
                  PriceB = Some dayB.Price }
            Some output)
// or, using set which I know you do not know. But #yolo
let timesA = stockA |> List.map(fun x -> x.Time) |> set
let timesB = stockB |> List.map(fun x -> x.Time) |> set
let timesAandB = Set.intersect timesA timesB
let tslC2 =
    timesAandB
    |> Set.toList
    |> List.map(fun time -> 
        { Time = time 
          PriceA = Some stockaByTime.[time].Price
          PriceB = Some stockbByTime.[time].Price})

// 4.
// 
let stockAByTime = 
    stockA 
    |> List.map(fun day -> day.Time, day)
    |> Map.ofList
 
let stockBByTime = 
    stockB 
    |> List.map(fun day -> day.Time, day)
    |> Map.ofList
let stockATimes = stockA |> List.map(fun x -> x.Time)
let stockBTimes = stockB |> List.map(fun x -> x.Time)
let allTimes = 
    List.append stockATimes stockBTimes
    |> List.distinct
let tslD =
    allTimes
    |> List.map(fun time ->
        let a = 
            match Map.tryFind time stockAByTime with
            | None -> None
            | Some a -> Some a.Price
            
        let b = 
            // same thing as what's done above with match expression,
            // but with Option.map. Personal preference depending
            // on what seems clearest. If you're mapping an option
            // and returning None for the None case,
            // a Option.map can be nice.
            Map.tryFind time stockBByTime
            |> Option.map(fun b -> b.Price)
        { Time = time; PriceA = a; PriceB = b })       
        
// or, using a function. This is the same thing as in the above
// anonymous lambda function, but I'm going to use different 
// code to achieve the same goal just to show you different options.
// again, it's just personal preference.
// check how to write the function using time = 1 as a test
let testTime = 1
let time1A = Map.tryFind testTime stockaByTime
let time1B = Map.tryFind testTime stockbByTime
let time1Aprice = time1A |> Option.map(fun x -> x.Price )
let time1Bprice = time1B |> Option.map(fun x -> x.Price)
let testOutput = { Time = testTime; PriceA = time1Aprice; PriceB = time1Bprice }
// now turn above code into a function to do the same thing
let getTheMatch time =
    let a = Map.tryFind time stockaByTime
    let b = Map.tryFind time stockbByTime
    let aPrice = a |> Option.map(fun x -> x.Price)
    let bPrice = b |> Option.map(fun x -> x.Price)
    { Time = testTime; PriceA = aPrice; PriceB = bPrice }
// test it to see that it works
getTheMatch 1
getTheMatch 2
// now do it for the whole list
let tslD2 = allTimes |> List.map getTheMatch    
