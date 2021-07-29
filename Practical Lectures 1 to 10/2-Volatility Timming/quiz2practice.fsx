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
|> Seq.filter (fun x -> x > 2 )
(*
2- Given a the list below, take elements until you find one that is greater than 4.

[ 1; -4; 7; 2; -10]

*)
[ 1; -4; 7; 2; -10]
|> Seq.takeWhile (fun x -> x < 4 )
(*
3 - Given a the list below, skip elements until you find one that is greater than 4.

[ 1; -4; 7; 2; -10]

*)
[ 1; -4; 7; 2; -10]
|> Seq.skipWhile (fun x -> x < 4)
(*
4 - Given a the list below, return tuples of all consecutive pairs.

[ 1; -4; 7; 2; -10]

*)
[ 10 .. 20]
|> List.pairwise
[ 1; -4; 7; 2; -10]
|> List.pairwise  
(*
5 - Given a the list below, return sliding windows of 3 consecutive observations.

[ 1; -4; 7; 2; -10]

*)
[ 1 .. 10 ]
|> List.windowed 4
[ 1; -4; 7; 2; -10]
|> List.windowed 3
// [ 1; -4; 7; 2; -10]
// |> List.windowed 2
// |> List.windowed 2
(*
6 - Given a the list below, use scan to return the intermediate and final cumulative sums.

[ 1; -4; 7; 2; -10]

*)
(0, [ 1; -4; 7; 2; -10])
||> List.scan (fun acc x -> acc + x)

(0, [ 1; -4; 7; 2; -10])
||> List.scan (( + ))
(*
7 - Given a the list below, use fold to return the final sum.

[ 1; -4; 7; 2; -10]

*)
(0, [ 1; -4; 7; 2; -10])
||> List.fold (fun state x -> state + x)
(0, [ 1; -4; 7; 2; -10])
||> List.fold (( + ))

(*
8 - Given a the list below, use mapFold to return the intermediate and final cumulative sums.

[ 1; -4; 7; 2; -10]

*)
(0, [ 1; -4; 7; 2; -10])
||> List.mapFold (fun acc x -> acc + x, acc + x) // this one starts with the first element of the list, 1
(0, [ 1; -4; 7; 2; -10])
||> List.mapFold (fun state x -> state, state + x) // this one starts from zero

(*
9 - Given a the list below, use mapFold to return
- The initial state + 1 as a string
- The final cumulative sums of the initial states.

[ 1; -4; 7; 2; -10]

*)
(0, [ 1; -4; 7; 2; -10])
||> List.mapFold (fun acc x -> string (x + 1), acc + x) // This picks up the x, the initial state, and sums 1
// (0, [ 1; -4; 7; 2; -10])
// ||> List.mapFold (fun state x -> string(state+1), state + x) // This is grabbing the intermediate sums
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
||> List.mapFold (fun acc x -> { x with Y = x.Y + 1; X = x.X + " batatas"}, acc + x.Y) // THis returns a tuple with ([{X, Y+1}], Final Sum)

(0, r1xs)
||> List.mapFold (fun state x -> x.Y + 1, state + x.Y) // THis returns a tuple with ([Y+1], Final Sum)
(*
11 - Given a the list below, sum all the elements.

[ 1; -4; 7; 2; -10]

*)
[ 1; -4; 7; 2; -10]
|> List.sum

[ -100; -4; -7; 12]
|> List.sum

(*
12 - Given a the list below, add 1 to all the elements and then calculate the sum.

[ 1; -4; 7; 2; -10]

*)

[ 1; -4; 7; 2; -10]
List.sumBy (fun x -> x + 1) 

[ 1; -4; 7; 2; -10]
|> List.map (fun x -> x + 1)
|> List.sum
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
[ 1.0; -4.0; 7.0; 2.0; -10.0]
|> List.averageBy decimal

[ 1.0; -4.0; 7.0; 2.0; -10.0]
|> List.averageBy (fun x -> decimal(x))

type R2 = { A : float; B : float }

let r2xs =

    [ { A = 1.0; B = 2.0 }

      { A = 1.0; B = 4.0 }

      { A = 1.0; B = -10.0 }]

(0.0, r2xs)
||> List.mapFold (fun ac x -> { x with B = x.B**2.0}, ac + x.A)
