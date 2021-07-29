let x = 1
x + 1

let results =
    let yearAgo = 100.0
    let today = 120.0
    (today / yearAgo) - 1.0

let calcReturn pv fv = 
    (fv / pv) - 1.0

calcReturn 11. 12.0
calcReturn yearAgo today

let calcReturnDecimal pv fv = 
    (fv / pv) - 1.0m

let simpleReturn beginningPrice endingPrice dividend =
    (endingPrice + dividend)/beginningPrice - 1.0

simpleReturn 40.0 45.0 1.0 

let logReturn beginningPrice endingPrice dividend =
    (endingPrice + dividend)/log(beginningPrice)

logReturn 40.0 45.0 1.0