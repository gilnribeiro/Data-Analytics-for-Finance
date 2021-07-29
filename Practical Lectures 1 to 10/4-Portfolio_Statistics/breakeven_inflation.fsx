
(**
# Breakeven inflation rates?
I had a question where a student asked how to interpret
rising interest rates. What is changing?

I thought this would be a good way to do some Fred exploration.

One way that you can get an idea is by decomposing nominal interest rates
into real interest rates and break-even inflation rates.
The idea with breakeven inflation is what is the inflation rate 
that will make you indifferent between
buying a nominal bond and buying an inflation protected bond (real bond).
Inflation protected bonds pay you a real interest rate + a top up for
whatever the inflation rate is.
Nominal bonds pay you a nominal rate; expected inflation and inflation rate risk
is built into the nominal rate.

You can think of the breakeven rate as (1+nominal)=(1+real)(1+breakeven inflation)
If the breakeven inflation part goes up/down, that is caused by expected inflation
going up/down or inflation risk going up/down.

Let's see how to examine this.
*)

#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET, 2.0.0-beta8"

open System
open FSharp.Data
open FSharp.Stats
open Plotly.NET

type Fred = CsvProvider<"https://fred.stlouisfed.org/graph/fredgraph.csv?id=GS10",
                         Schema="Date,Value (float)",
                         MissingValues=".">
let fredUrl series = $"https://fred.stlouisfed.org/graph/fredgraph.csv?id={series}"

// 10-year inflation protected US bond (real 10-year rate)
let real = Fred.Load(fredUrl "DFII10")
// A regular 10-year US bond (nominal 10-year rate)
let nominal = Fred.Load(fredUrl "DGS10")

(**
We want to calculate the breakeven rate.
The idea is what inflation rate makes you indifferent between
the real and nominal bond.
We can say, let's go through the nominal days and find
the real rate on that day. Then we'll do
*)

let breakevenRate nominalRate realRate = 
    let nomPct = nominalRate / 100.0
    let realPct  = realRate / 100.0
    // Now also account for quoting yields as an APR
    // with semi-annual compounding.
    // This means if yield is y, then if you invest $1 then at the
    // end of the year you get $1*(1+y/2)^2.
    // 
    let nom1yReturn = (1.0 + nomPct/2.0)**2.0
    let real1yReturn = (1.0 + realPct/2.0)**2.0
    let breakeven = (nom1yReturn/real1yReturn) - 1.0
    breakeven

// How do we look up a real rate on a particular day? Put it in a Map collection
// where the date is the key and the value is the interest rate on that day.
let realMap =
    real.Rows 
    |> Seq.map(fun day -> day.DATE, day.Value)
    |> Map.ofSeq

// Now we can go through all the nominal days and get the breakeven
// rate on that day
let breakeven =
    nominal.Rows
    |> Seq.map(fun nominalDay ->
        let dt = nominalDay.DATE
        let realThisDay =
            // one thing to keep in mind is that we might look up a day
            // that doesn't exist in the realMap.
            // To prevent an error, we should use Map.tryFind
            // and then explicitly handle the cases where we did not find
            // a real rate on that day
            match Map.tryFind dt realMap with 
            | None -> nan // if we didn't find a real reate on that day return nan (Not a Number)
            | Some rate -> rate // if we found a rate return that day
        let breakeven = breakevenRate nominalDay.Value realThisDay // use our function
        dt, breakeven*100.0)
    |> Seq.filter(fun (date, breakeven) -> date > DateTime(2018,1,1))    
    |> Seq.toArray

let breakevenChart =
    breakeven
    |> Chart.Line
    |> Chart.withTraceName "Breakeven Inflation"

let realChart =
    real.Rows
    |> Seq.filter(fun day ->  day.DATE > DateTime(2018,1,1))
    |> Seq.map(fun day -> day.DATE,day.Value)
    |> Chart.Line
    |> Chart.withTraceName "Real Rate"

(**
Lets do the same thing with nominal rates, but let's make a function to do it.
copy and paste realChart code from above, but we'll replace "real" with a variable named "data" 
and replace "Real Rate" with a variable named "name"
Addtionally, we'll give a type hint for data so that it knows what it is. Otherwise
F# is not sure what data structure the data variable represents. This is an example where
type inference is not figuring things out, so we give it this "Fred" type hint by specifying
the type of the variable that we're calling "data".
*)

let makeAnotherChart (data: Fred) name =
    data.Rows
    |> Seq.filter(fun day ->  day.DATE > DateTime(2018,1,1))
    |> Seq.map(fun day -> day.DATE,day.Value)
    |> Chart.Line
    |> Chart.withTraceName name


let nominalChart = makeAnotherChart nominal "Nominal Rate"

let combined = Chart.Combine([breakevenChart; realChart; nominalChart ])

combined |> Chart.Show
// With the chart, think of it like nominal rates 
// are a combination of real rates and breakeven inflation. 

// It appears that it is mostly breakeven inflation pushing nominal rates up,
// and breakeven inflation = expected inflation + inflation risk. 
// So mostly, people expect more inflation so inflation has gone up.

// We can also compare our calculation to FRED's breakeven calculation

let fredsBreakeven = Fred.Load(fredUrl "T10YIE")

let myBreakevenMap = breakeven |> Map.ofSeq

open Correlation // from FSharp.Stats

(**   
Seq.unzip is a function that lets you convert
a List/Array/Sequence of tuple pairs like
[(1,2); (3,4); (5,6)] into
a tuple of the fst and snd values
For example,
Seq.unzip [(1,2); (3,4); (5,6)]
gives ([1; 3; 5], [2; 4; 6])

Here, afer I do Seq.unzip I get a tuple where the first
thing in the tuple is a sequence of all of FRED's breakeven rates
and the second thing in the tuple is a sequence of all my rates.
*)

let breakevenPairs =
    fredsBreakeven.Rows
    |> Seq.map(fun fredsDay ->
        let myDay = 
            match Map.tryFind fredsDay.DATE myBreakevenMap with
            | None -> nan
            | Some rate -> rate 
        fredsDay.Value, myDay)
    |> Seq.filter(fun (fred,me) -> 
        not (Double.IsNaN fred) && not (Double.IsNaN me))    
    |> Seq.unzip

// pattern matching to deconstruct the tuple
// same thing as
// let a, b = (1, 2)
// except now 1 and 2 are sequences of interest rates
let fredsBreakevenRates, myBreakevenRates = breakevenPairs
// Pearson correlation
// pearson fredsBreakevenRates, myBreakevenRates
fredsBreakevenRates |> Seq.pearson myBreakevenRates
// Plot them
let fredsBreakevenChart = makeAnotherChart fredsBreakeven "Fred's Breakeven"

let comparisonChart = Chart.Combine([breakevenChart; fredsBreakevenChart ])

comparisonChart |> Chart.Show