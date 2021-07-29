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
|> Array.collect(fun x -> x |> Array.map(fun x -> x + 1.0))

[| [| 1.0; 2.0|]
   [| 3.0; 4.0|] |]
|> Array.map(fun x -> x |> Array.map(fun y -> y + 1.0))
|> Array.concat

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
let add2 add = add + 2 

add2 (add 1 2)

let add3 = (+) 3
add 2 2 |> add2


(*
    Given `returns : ReturnOb []`, use printfn to print the whole
    array to standard output using the [structured plaintext formatter](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/plaintext-formatting). 
*)
returns |> (printfn "%A")
printfn "%A" returns
printfn "%A" (returns : ReturnOb [])
printfn "%A" [for i in (returns : ReturnOb []) -> (i)]
(*
    Given `returns : ReturnOb []`, use printfn to print each record
    to standard output using the [structured plaintext formatter](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/plaintext-formatting). 
*)
for i in returns do
    printfn $"{i}"


returns |> Array.iter (printfn "%A")
(*
    Given the tuple ("hi", false, 20.321, 4),
    use printfn and the tuple to print the following string
    to standard output:

    "hi teacher, my False knowledge implies that 4%=0020.1"

    [String formatting](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/plaintext-formatting#format-specifiers-for-printf) documentation will be useful. 
*)
let (hi, F, FlO, IT) = ("hi", false, 20.321, 4) 
printfn $"{hi} teacher, my {F} knowledge implies that {IT}%%=%030.1f{FlO}"

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
|> Array.groupBy(fun x -> x.Symbol, x.Date.Month)
|> Array.map(fun ((symbol, month_), xs) -> 
    let mtreturn = (1.0, xs) ||> Array.fold(fun acc x -> acc*(1.0 + x.Return))
    { Symbol = symbol 
      Date = xs |> Array.map(fun x -> x.Date) |> Array.max
      Return = mtreturn })
(*
    Given `returns : ReturnOb []`, calculate the standard deviation of daily returns
    for every symbol each month.
    Give the result as a `ValueOb []` where the date in each `ValueOb` is the last date for the symbol
    each month. 
*)


returns
|> Array.groupBy(fun x -> x.Symbol, x.Date.Year, x.Date.Month)
|> Array.map(fun((symbol, year_, month_ ), xs) -> 
    let stD = xs |> stDevBy(fun x -> x.Return)
    { Symbol = symbol 
      Date = xs |> Array.map(fun x -> x.Date) |> Array.max
      Value = stD }
    )
(*
    Given `returns : ReturnOb []`, calculate the standard deviation of daily returns
    for every symbol using rolling 3 day windows.
    Give the result as a `ValueOb []` where the date in each `ValueOb` is the last date for the symbol
    in the window. 
*)
returns
|> Array.groupBy(fun x -> x.Symbol, x.Date.Year, x.Date.Month)
|> Array.windowed 3
|> Array.collect(fun x -> 
    x |> Array.map(fun ((symbol, year_, month_), xs) -> 
        let last = xs |> Array.last
        let stdev = xs |> stDevBy(fun x -> x.Return)
        { Symbol = last.Symbol
          Date = last.Date
          Value = stdev }
        ))

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