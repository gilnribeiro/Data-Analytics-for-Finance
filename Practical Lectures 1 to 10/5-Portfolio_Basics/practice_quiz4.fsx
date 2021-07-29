/////////
// Joins
/////////
(*
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

(*1. Create a `TwoStocksPriceOb list` named `tslA` that has prices for
every observation of stockA. If there is a price for stockB
at the same time as stockA, then include the stockB price. Otherwise,
the stockB price should be None.
*)
let mapB = 
    stockB 
    |> List.map(fun x -> x.Time, x)
    |> Map.ofList

let tslA = 
    stockA
    |> List.map(fun x -> 
        let timeB = Map.tryFind x.Time mapB
        match timeB with
        | Some tB -> { Time = x.Time ; PriceA = Some x.Price ; PriceB = Some tB.Price}
        | None -> { Time = x.Time ; PriceA = Some x.Price ; PriceB = None}
        )

(*2. Create a `TwoStocksPriceOb list` named `tslB` that has prices for
every observation of stockB. If there is a price for stockA
at the same time as stockB, then include the stock A price. Otherwise,
the stockA price should be None.
*)
let mapA = stockA |> List.map(fun x -> x.Time, x) |> Map.ofList 
let tslB = 
    stockB
    |> List.map(fun x -> 
        let timeA = Map.tryFind x.Time mapA
        match timeA with
        | Some tA -> { Time = x.Time ; PriceA = Some tA.Price ; PriceB = Some x.Price}
        | None -> { Time = x.Time ; PriceA = None ; PriceB = Some x.Price}
        )
(*3. Create a `TwoStocksPriceOb list` named `tslC` that only includes times
when there is a price for both stockA and stockB. The prices for stocks
A and B should always be something.
*)
let tslC =
    stockB
    |> List.choose(fun x -> 
        let timeA = Map.tryFind x.Time mapA
        match timeA with 
        | Some tA -> Some { Time = x.Time ; PriceA = Some tA.Price ; PriceB = Some x.Price}
        | None -> None
        )

(*4. Create a `TwoStocksPriceOb list` named `tslD` that includes available
stock prices for stockA and stockB at all possible times. If a price for
one of the stocks is missing for a given time, it should be None.
*)
let timeA = stockA |> List.map(fun x -> x.Time) |> Set
let timeB = stockB |> List.map(fun x -> x.Time) |> Set
let timeAB = timeA |> Set.union timeB
let tslD = 
    [for i in timeAB do
        let timAB = (Map.tryFind i mapA, Map.tryFind i mapB) 
        match timAB with
        | (Some tA, Some tB) -> { Time = i ; PriceA = Some tA.Price ; PriceB = Some tB.Price}
        | (Some tA, None) -> { Time = i ; PriceA = Some tA.Price ; PriceB = None}
        | (None, Some tB) -> { Time = i ; PriceA = None ; PriceB = Some tB.Price} 
        | (None , None) -> { Time = i ; PriceA = None ; PriceB = None}
    ]

    