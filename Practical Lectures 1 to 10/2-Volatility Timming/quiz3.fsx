(**
We're going to use the following in the questions
*)
#r "nuget: FSharp.Stats"

open System
open FSharp.Stats

fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

type ReturnOb = { Symbol : string; Date : DateTime; Return : float }
type ValueOb = { Symbol : string; Date : DateTime; Value : float }

let seed = 1
Random.SetSampleGenerator(Random.RandBasic(seed))   
let normal = Distributions.Continuous.normal 0.0 0.1

let returns =
    [| 
        for symbol in ["AAPL"; "TSLA"] do
        for month in [1..2] do
        for day in [1..3] do
            { Symbol = symbol 
              Date = DateTime(2021, month, day)
              Return = normal.Sample()}
    |]
(**
////////
Questions 
////////
*)
(*
    Take this array of arrays, add 1.0 to each element of the "inner" arrays,
    and then concatenate all the inner arrays together.
    [| [| 1.0; 2.0|]
       [| 3.0; 4.0|] |]
*)
[| [| 1.0; 2.0|]
   [| 3.0; 4.0|] |]
|> Array.map (fun x -> x |> Array.map (fun x -> x + 1.0))
|> Array.concat

[| [| 1.0; 2.0|]
   [| 3.0; 4.0|] |]
|> Array.collect (fun x -> x |> Array.map (fun x -> x + 1.0))

[| [| 1.0; 2.0|]
   [| 3.0; 4.0|] |]
|> Array.concat
|> Array.map (fun x -> x + 1.0)
(*
    Take the following two-parameter function:
    ```
        let add x y = x + y
    ```
    Use the above function and [partial application](https://fsharpforfunandprofit.com/posts/partial-application/)
    to define a new function called 
    `add2` that adds 2 
    to it's input.
*)
let add x y = x + y
let add3 = add 3
// let add2 = (+) 2
// add 2 2 |> add2
(*
    Given `returns : ReturnOb []`, use printfn to print the whole
    array to standard output using the [structured plaintext formatter](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/plaintext-formatting). 
*)
/// ITS ALL THE SAME
returns |> (printfn "%A")
printfn "%A" returns
printfn "%A" (returns : ReturnOb [])
printfn "%A" [for i in (returns : ReturnOb []) -> (i)]
(*
    Given `returns : ReturnOb []`, use printfn to print each record
    to standard output using the [structured plaintext formatter](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/plaintext-formatting). 
*)
// returns : ReturnOb []

returns |> Array.iter (printfn "%A")
(*
    Given the tuple ("hi", false, 20.321, 4),
    use printfn and the tuple to print the following string
    to standard output:

    "hi teacher, my False knowledge implies that 4%=0020.1"

    [String formatting](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/plaintext-formatting#format-specifiers-for-printf) documentation will be useful. 
*)
let (xString, xBool, xFloat, xInt) = ("hi", false, 20.321, 4)
/// New Style // Using string interpolation
printfn $" {xString} teacher, my {xBool} knowledge implies that {xInt}%%=%06.1f{xFloat} "
printfn $" %06.1f{xFloat} "
/// Old Style
printfn "%s teacher, my %b knowledge implies that %d%%=%06.1f" xString xBool xInt xFloat
(*
    Given `returns : ReturnOb []`, calculate the arithmetic average return 
    for every symbol each month.
    Give the result as a `ReturnOb []` where the date is the last date for the symbol
    each month. 
*)
returns
|> Array.groupBy (fun x -> x.Symbol, x.Date.Year, x.Date.Month)
|> Array.map (fun ((symbol, year_, month_), xs) ->  
    let avgRet = xs |> Array.averageBy (fun x -> x.Return)
    {Symbol = symbol
     Date = xs |> Array.map (fun x -> x.Date) |> Array.max
     Return = avgRet})
(*
    Given `returns : ReturnOb []`, calculate the monthly return 
    for every symbol each month.
    Give the result as a `ReturnOb []` where the date is the last date for the symbol
    each month. 
*)
returns
|> Array.groupBy (fun x -> x.Symbol, x.Date.Year, x.Date.Month)
|> Array.map (fun ((symbol, year_, month_), xs) ->
    let monthReturnPlus1 = (1.0, xs) ||> Array.fold (fun acc x -> acc*(1.0 + x.Return))
    { Symbol = symbol
      Date = xs |> Array.map(fun x -> x.Date) |> Array.max
      Return = monthReturnPlus1 - 1.0}) 
(*
    Given `returns : ReturnOb []`, calculate the standard deviation of daily returns
    for every symbol each month.
    Give the result as a `ValueOb []` where the date in each `ValueOb` is the last date for the symbol
    each month. 
*)
returns
|> Array.groupBy (fun x -> x.Symbol, x.Date.Year, x.Date.Month)
|> Array.map (fun ((symbol, year_, month_), xs) ->  
    let sd = xs |> stDevBy (fun x -> x.Return)
    { Symbol = symbol
      Date = xs |> Array.map (fun x -> x.Date) |> Array.max
      Value = sd})

returns
|> Array.groupBy(fun x -> x.Symbol, x.Date.Year, x.Date.Month)
|> Array.map(fun ((symbol, _year, _month), xs) ->
    { Date = xs |> Array.map(fun x -> x.Date) |> Array.max 
      Symbol = symbol
      Value =  xs |> stDevBy(fun x -> x.Return) })
(*
    Given `returns : ReturnOb []`, calculate the standard deviation of daily returns
    for every symbol using rolling 3 day windows.
    Give the result as a `ValueOb []` where the date in each `ValueOb` is the last date for the symbol
    in the window. 
*)

/// My Way
returns
|> Array.groupBy (fun x -> x.Symbol, x.Date.Year, x.Date.Month)
|> Array.windowed 3
|> Array.collect (fun xs -> 
    xs |> Array.map (fun ((symbol, year_, month_), xs) ->
        let last = xs |> Array.last
        let sd = xs |> stDevBy (fun x -> x.Return)
        {   Symbol = last.Symbol
            Date = last.Date
            Value = sd}))

/// Teacher's Way
returns
|> Array.groupBy (fun x -> x.Symbol)
|> Array.collect (fun (symbol, xs) ->
    xs
    |> Array.sortBy (fun x -> x.Date)
    |> Array.windowed 3
    |> Array.map (fun ys -> 
    let last = ys |> Array.last
    { Symbol = last.Symbol
      Date = last.Date
      Value = ys |> stDevBy (fun x -> x.Return)}))



 

let example = [("a",1.0); ("a",1.5);("b",3.0);("b",3.25);("c",2.0)]
example
|> List.groupBy(fun (x, y) -> x)
|> List.map (fun (x,y) -> y |> List.map snd |> List.average)

[| [| "a"; "b"|]
   [| "d"; "c"|] |]
|> Array.map (fun x -> x |> Array.map (fun x -> x))
|> Array.concat

returns
|> Array.groupBy (fun x -> x.Date)
|> Array.map (fun (x, y) -> 
    y
    |> Array.averageBy (fun y -> y.Return))






