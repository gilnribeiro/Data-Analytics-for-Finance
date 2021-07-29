//////////////////////
// NESTED FUNCTIONS //
//////////////////////
let addThreeNumbers x y z = 
    // create a nested helper function
    let add n =
        fun x -> x + n
    // use the helper function
    x |> add y |> add z

addThreeNumbers 2 3 5

let validateSize max n  =

    //create a nested helper function with no params
    let printError() =
        printfn "Oops: '%i' is bigger than max: '%i'" n max

    // use the helper function
    if n > max then printError()

// test
validateSize 10 9
validateSize 10 11

let sumNumbersUpTo max =

    // recursive helper function with accumulator
    let rec recursiveSum n sumSoFar =
        match n with
        | 0 -> sumSoFar
        | _ -> recursiveSum (n-1) (n+sumSoFar)

    // call helper function with initial values
    recursiveSum max 0

// test
sumNumbersUpTo 10

/////////////
// MODULES //
/////////////

module MathStuff = 
    let add x y = x + y
    let subtract x y = x - y

module OtherStuff = 
    // Use a function from the MathStuff module
    let add1 x = MathStuff.add x 1

// we can also import all the funtions in another module with the open directive
module OtherStuff1 = 
    open MathStuff //make all functions available
    let add1 x = add x 1
    let sub1 x = subtract x 1

////////////////////
// NESTED MODULES //
////////////////////

module MathStuff1 = 
    let add x y = x + y
    let subtract x y = x - y
    // nested module
    module FloatLib = 
        let add x y : float = x + y
        let subtract x y : float = x - y

// And other modules can reference functions in the nested modules using either a full name or a relative name as appropriate:

module OtherStuff2 =
    open MathStuff1
    let add1 x = add x 1
    // fully qualified
    let add1Float x = MathStuff1.FloatLib.add x 1.0
    // with a relative paht
    let sub1Float x = FloatLib.subtract x 1.0

// A module can contain other declarations as well as functions, including type declarations, simple values and initialization code (like static constructors)

module MathStuff3 =

    // functions
    let add x y  = x + y
    let subtract x y  = x - y

    // type definitions
    type Complex = {r:float; i:float}
    type IntegerFunction = int -> int -> int
    type DegreesOrRadians = Deg | Rad

    // "constant"
    let PI = 3.141

    // "variable"
    let mutable TrigType = Deg

    // initialization / static constructor
    do printfn "module initialized"