open System
// Loops and iteration
// Forward piping |> 
// Collection Functions

let sayHello person =
    printfn "Hello %s from my F# program!" person

let isValid person = 
    String.IsNullOrWhiteSpace person |> not

let isAllowed person = 
    person <> "Eve"

let main1 argv = 
    argv 
    |> Array.filter isValid 
    |> Array.filter isAllowed
    |> Array.iter sayHello
    printfn "Nice to meet you"
    0
 
main1 [|"alice";"";"Eve";"Bob"|]
