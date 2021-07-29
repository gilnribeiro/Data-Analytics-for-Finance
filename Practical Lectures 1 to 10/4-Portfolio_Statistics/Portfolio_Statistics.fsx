(**
# Portfolio Statistics

Working towards portfolio optimization.

*)

#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET, 2.0.0-beta5"
open System
open System.IO
open FSharp.Data
open FSharp.Stats
open Plotly.NET

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

let cacheDirectory = "../data-cache/"

(**
### Modules are a way to organize related functions
[FSharpForFunAndProfit](https://fsharpforfunandprofit.com/posts/organizing-functions/) has a good discussion of modules
*)

module ExModule =
    let add2 x = x + 2

// Now we're outside the module
ExModule.add2 4

(**
If we look at `../Common.fsx`, we can see some functions in modules.
*)

#load "../Common.fsx"
open Common


(**
Let's use the Fred module from Common.
*)

type ReturnOb = { Symbol : string ; Date : DateTime; Return : float }
let rates10y = Fred.get "DGS10"
// Are any of these missing?
rates10y.Rows
|> Seq.countBy(fun x -> x.DATE.Year, x.Value.isNone)
|> Seq.filter(fun ((year, isNone), count) -> isNone)
|> Seq.toList
// Yup. FYI, missing values for FRED are mostly daily series.

// Let's create a cleaned version where we have the last valid yield per month.
let rates10yCleanMonthly =
    rates10y.Rows
    |> Seq.choose(fun row -> 
        // Choose is a bit like a filter that allows you to do a map inside
        // It discards elements where the function evaluates to None,
        // and when the function evaluates to (Some x) it returns x.
        match row.Value with
        | None -> None
        | Some rate -> Some (row.DATE, rate)) 
    |> Seq.groupBy(fun (date, rate) -> date.Year, date.Month)
    |> Seq.map(fun (month, daysInMonth) ->
        daysInMonth
        |> Seq.sortBy fst
        |> Seq.last)
