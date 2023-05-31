module day03

open System
open Xunit
open Common
open FsUnit

let visitNextHouse s x =
    let last = s |> List.head

    let next =
        match x with
        | '^' -> (fst last, snd last + 1)
        | '>' -> (fst last + 1, snd last)
        | '<' -> (fst last - 1, snd last)
        | 'v' -> (fst last, snd last - 1)
        | c -> failwithf "invalid char %c" c

    next :: s

let getListOfVisitedHouses = Array.fold visitNextHouse [ (0, 0) ]

let countDistinctHouses xs=
    xs
    |> getListOfVisitedHouses
    |> Set.ofList
    |> Set.count

[<Fact>]
let solve1 () =
    "../../../Solutions/03/data.txt"
    |> parseFirstLine asCharArray
    |> countDistinctHouses
    |> should equal 2572

[<Fact>]
let solve2 () =
    "../../../Solutions/03/data.txt"
    |> parseFirstLine asCharArray
    |> Array.indexed
    |> Array.partition (fun (i,c) -> i % 2 = 0)
    |> fun t -> [t |> fst |> Array.map snd; t |> snd |> Array.map snd]
    |> List.map getListOfVisitedHouses
    |> List.collect id
    |> Set.ofList
    |> Set.count
    |> should equal 2631
