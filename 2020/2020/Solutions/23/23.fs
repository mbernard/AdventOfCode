module _23

open System
open System.Collections
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharpx.Collections
open Xunit
open Common


let parse (s: string) =
    s.ToCharArray() |> Array.map (string >> int)

let rec selectDest x (picks: int list) (xs: IDictionary<int, int>) max =
    if x = 0 then selectDest max picks xs max
    else if List.contains x picks then selectDest (x - 1) picks xs max
    else x

let play (c, xs: Dictionary<int, int>, max) round =
    let picks =
        [ xs.[c]
          xs.[xs.[c]]
          xs.[xs.[xs.[c]]] ]

    let dest = selectDest (c - 1) picks xs max
    xs.[c] <- xs.[picks.[2]]
    xs.[picks.[2]] <- xs.[dest]
    xs.[dest] <- picks.[0]

    (xs.[c], xs, max)

let toDict xs =
    xs
    |> Array.pairwise
    |> dict
    |> Dictionary<int, int>
    |> (fun x ->
        x.Add(Array.last xs, xs.[0])
        x)

let rec score c (xs: Dictionary<int, int>) =
    if c = 1 then String.Empty else string c + score xs.[c] xs

let solve1 moves input =
    let cups = input |> parse
    let game = cups |> toDict

    [ 1 .. moves ]
    |> Seq.fold play (cups.[0], game, 9)
    |> (fun (_, xs, _) -> score xs.[1] xs)

[<Theory>]
[<InlineData("92658374", 10, "389125467")>]
[<InlineData("67384529", 100, "389125467")>]
[<InlineData("45798623", 100, "398254716")>]
let ``Solve 1`` expected moves input =
    let res = solve1 moves input
    Assert.Equal(expected, res)

let generate xs = Array.append xs [| 10 .. 1_000_000 |]

let score2 x (xs:Dictionary<int,int>) =
    (int64 x) * (int64 xs.[x])

let solve2 moves input =
    let cups = input |> parse |> generate
    let game = cups |> toDict

    [ 1 .. moves ]
    |> Seq.fold play (cups.[0], game, 1_000_000)
    |> (fun (_, xs, _) -> score2 xs.[1] xs)


[<Theory>]
[<InlineData(149245887792L, 10_000_000, "389125467")>]
[<InlineData(235551949822L, 10_000_000, "398254716")>]
let ``Solve 2`` expected moves input =
    let res = solve2 moves input
    Assert.Equal(expected, res)
