(**
////////
Questions 
////////
*)
(*
1 - Given a the list below, filter the list so that only numbers great than 2 remain.

[ 1; -4; 7; 2; -10]
*)
[ 1; -4; 7; 2; -10]
|> List.filter(fun x -> x > 2)
(*
2- Given a the list below, take elements until you find one that is greater than 4.

[ 1; -4; 7; 2; -10]
*)
[ 1; -4; 7; 2; -10]
|> List.takeWhile(fun x -> x <= 4)
(*
3 - Given a the list below, skip elements until you find one that is greater than 4.

[ 1; -4; 7; 2; -10]
*)
[ 1; -4; 7; 2; -10]
|> List.skipWhile(fun x -> x <= 4)
(*
4 - Given a the list below, return tuples of all consecutive pairs.

[ 1; -4; 7; 2; -10]
*)
[ 1; -4; 7; 2; -10]
|> List.pairwise
(*
5 - Given a the list below, return sliding windows of 3 consecutive observations.

[ 1; -4; 7; 2; -10]
*)
[ 1; -4; 7; 2; -10]
|> List.windowed 3
(*
6 - Given a the list below, use scan to return the intermediate and final cumulative sums.

[ 1; -4; 7; 2; -10]
*)
(0, [ 1; -4; 7; 2; -10])
||> List.scan(fun acc x -> acc + x)
(0, [ 1; -4; 7; 2; -10])
||> List.scan((+))
(*
7 - Given a the list below, use fold to return the final sum.

[ 1; -4; 7; 2; -10]
*)
(0, [ 1; -4; 7; 2; -10])
||> List.fold(fun acc x -> acc + x)
(*
8 - Given a the list below, use mapFold to return the intermediate and final cumulative sums.

[ 1; -4; 7; 2; -10]
*)
(0, [ 1; -4; 7; 2; -10])
||> List.mapFold(fun acc x -> acc, acc + x)
(*
9 - Given a the list below, use mapFold to return
- The initial state + 1 as a string
- The final cumulative sums of the initial states.

[ 1; -4; 7; 2; -10]
*)
(0, [ 1; -4; 7; 2; -10])
||> List.mapFold(fun acc x -> string(x + 1), acc + x)
(*
10 - Given a the list below, use mapFold to return a tuple of
- The list of records with the Y field in each record updated to Y+1
- The sum of the Y fields.
type R1 = { X : string; Y : int }

let r1xs =
    [ { X = "a"; Y = 1 }
      { X = "b"; Y = -4 }
      { X = "c"; Y = 7 } 
      { X = "d"; Y = 2 }
      { X = "e"; Y = -10 }]
*)
type R1 = { X : string; Y : int }

let r1xs =
    [ { X = "a"; Y = 1 }
      { X = "b"; Y = -4 }
      { X = "c"; Y = 7 } 
      { X = "d"; Y = 2 }
      { X = "e"; Y = -10 }]

(0, r1xs)
||> List.mapFold(fun acc x -> {X = x.X ; Y = x.Y + 1}, acc + x.Y)

open System
type ReturnOb = { Symbol : string; Date : DateTime; Return : float }
let returns = 
    [| { Symbol = "AAPL"
         Date = DateTime(2021,1,1)
         Return = -0.02993466474 }
       { Symbol = "AAPL"
         Date = DateTime(2020,1,1)
         Return = 5.0299334383474 }
       { Symbol = "AAPL"
         Date = DateTime(2021,2,1)
         Return = -0.01872509079 }
       { Symbol = "TSLA"
         Date = DateTime(2021,3,1)
         Return = -0.1487757845 }
       { Symbol = "TSLA"
         Date = DateTime(2021,1,1)
         Return = 0.1059620976 } |]


(*
11 - Given a the list below, sum all the elements.

[ 1; -4; 7; 2; -10]
*)
[ 1; -4; 7; 2; -10]
|> List.sum
(*
12 - Given a the list below, add 1 to all the elements and then calculate the sum.

[ 1; -4; 7; 2; -10]
*)
(0, [ 1; -4; 7; 2; -10])
||> List.fold(fun acc x -> acc + x + 1)

[ 1; -4; 7; 2; -10]
|> List.sumBy(fun x -> x + 1)
(*
13 - Given a the list below, calculate the average of the elements in the list.

[ 1.0; -4.0; 7.0; 2.0; -10.0]
*)
[ 1.0; -4.0; 7.0; 2.0; -10.0]
|> List.average
(*
14 - Given a the list below, convert each element to a decimal and then calculate the average of the elements in the list.

[ 1.0; -4.0; 7.0; 2.0; -10.0]
*)
let first = 0m
(first, [ 1.0; -4.0; 7.0; 2.0; -10.0])
||> List.scan(fun acc x -> decimal(x))
|> List.skipWhile(fun x -> x = first)
|> List.average

[ 1.0; -4.0; 7.0; 2.0; -10.0]
|> List.averageBy decimal