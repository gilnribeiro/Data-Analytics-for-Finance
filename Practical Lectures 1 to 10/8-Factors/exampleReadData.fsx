#r "nuget:FSharp.Data"

open System
open FSharp.Data

fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

// set fsi working directory to script directory
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

// This script assumes the data .csv files are in the same
// folder as this script.
//
// If the data .csv files are in a different folder,
// then you will need to change the paths to correctly
// point to the location on your computer.
type IdAndReturnCsv = CsvProvider<Sample="id_and_return_data.csv",
                                  ResolutionFolder= __SOURCE_DIRECTORY__>

type Signal = CsvProvider<Sample="ret_12_1.csv",
                          ResolutionFolder= __SOURCE_DIRECTORY__>

let retm12m1 = Signal.Load(__SOURCE_DIRECTORY__ + "/ret_12_1.csv")
let zeroTrades252d = Signal.Load(__SOURCE_DIRECTORY__ + "/zero_trades_252d.csv")

let idAndReturns = IdAndReturnCsv.GetSample()

idAndReturns.Rows
|> Seq.map(fun x -> x.Id, x.Eom, x.RetExc)

retm12m1.Rows
|> Seq.map(fun x -> x.Id, x.Eom,x.Signal)

zeroTrades252d.Rows
|> Seq.map(fun x -> x.Id, x.Eom,x.Signal)