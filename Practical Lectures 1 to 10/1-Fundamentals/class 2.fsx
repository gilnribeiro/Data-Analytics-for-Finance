// ## Working with data

// With this foundation, let's now try loading some data. We are going to obtain and process the data using an external F# library called [FSharp.Data](https://github.com/fsprojects/FSharp.Data) that makes the processing easier. 

// ### Namespaces
// First, let's create a file directory to hold data. We are going to use built-in dotnet IO (input-output) libraries to do so.

// Set working directory to the this code file's directory
System.IO.Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)
// Now create cache directory one level above the working directory
let cacheDirectory = "../data-cache"
if not (System.IO.Directory.Exists(cacheDirectory))
then System.IO.Directory.CreateDirectory(cacheDirectory) |> ignore


#load "../secrets.fsx"
Secrets.tiigoKey

// Using External Libraries

// This will go to nuget and download this package
#r "nuget: FSharp.Data, 3.3.3"

open System
open System.IO
open FSharp.Data

// we are going to download some data from tiingo to a csv
let tiingoSampleFile = Path.Combine(cacheDirectory,"tiingo-sample.csv")

// The line below checks if the file already exists
// Here we are retrieve some data from the API
if not (File.Exists(tiingoSampleFile)) then
    Http.RequestString
            ( "https://api.tiingo.com/tiingo/daily/gme/prices", 
                httpMethod = "GET",
                query   = [ "token", Secrets.tiigoKey; 
                            "startDate", "2020-10-01";
                            "endDate", "2021-02-11";
                            "format","csv"],
                headers = [HttpRequestHeaders.Accept HttpContentTypes.Csv])
    |> fun x -> File.WriteAllText(tiingoSampleFile, x)   


let add1 x = x + 1.0
add1 1.0
let multiplyBy4 x = x*4.0
// This allows us to chain function
1.0 |> add1 |> multiplyBy4
// We can also define a funtion on the fly (an anonymous function)
2.0|> fun x -> x + 1.0 |> fun x -> x**10.0

// If you had not used - open System.IO above you'd have to use
// System.IO.File.ReadAllLines

// This reads the csv file
// The pipe function below "takes" a line from the array
File.ReadAllLines(tiingoSampleFile)
|> Array.take 1

let lines = File.ReadAllLines(tiingoSampleFile)
// First line
lines.[0]
// A couple of lines
lines.[1..3]

let xx = [| 1; 2; 34|]
// This sorta creates a range from 1 to 10
let xxx = [|1.0..10.0|]

xxx |> Array.average

// list
[1;2;3]
// squence
seq {1.0 .. 10.0}
// map
[|1.0 .. 5.0|]
// Basically: apply this function to each part of the array
|> Array.map add1