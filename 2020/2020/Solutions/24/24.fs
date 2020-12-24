module _24

open System
open System.Collections
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharpx.Collections
open Xunit
open Common

type Instruction =
    | E
    | SE
    | SW
    | W
    | NW
    | NE

type Color =
    | White
    | Black

let rec parseInstructions acc (c :: rest) =
    let i, r =
        match c with
        | 'e' -> E, rest
        | 'w' -> W, rest
        | 's' ->
            let c2 :: rest2 = rest

            match c2 with
            | 'e' -> SE, rest2
            | 'w' -> SW, rest2
        | 'n' ->
            let c2 :: rest2 = rest

            match c2 with
            | 'e' -> NE, rest2
            | 'w' -> NW, rest2

    if r = List.Empty then i :: acc else parseInstructions (i :: acc) r

let getCoord (c, r) ins =
    let isEvenRow = r % 2 = 0

    match ins with
    | E -> (c + 1, r)
    | W -> (c - 1, r)
    | NE -> if isEvenRow then (c + 1, r + 1) else (c, r + 1)
    | NW -> if isEvenRow then (c, r + 1) else (c - 1, r + 1)
    | SE -> if isEvenRow then (c + 1, r - 1) else (c, r - 1)
    | SW -> if isEvenRow then (c, r - 1) else (c - 1, r - 1)

let toCoordinates xs = xs |> List.fold getCoord (0, 0)

let solve1 data =
    data
    |> parseEachLine asCharArray
    |> Seq.map (Array.toList >> parseInstructions [])
    |> Seq.map toCoordinates
    |> Seq.countBy id
    |> Seq.filter (fun ((x, y), count) -> count % 2 <> 0)
    |> Seq.length

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/24/data.txt"
    Assert.Equal(269, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/24/data-test-1.txt"

    Assert.Equal(10, res)

let getNeighbors x =
    [ getCoord x E
      getCoord x W
      getCoord x SE
      getCoord x SW
      getCoord x NE
      getCoord x NW ]

let determineColor bt color tile =
    let blackCount =
        getNeighbors tile
        |> List.filter (fun x -> List.contains x bt)
        |> List.length

    let nextColor =
        match color with
        | Black -> if blackCount = 0 || blackCount > 2 then White else Black
        | White -> if blackCount = 2 then Black else White

    (tile, nextColor)

let flip bt day =
    let whiteNeighbors =
        bt
        |> List.collect getNeighbors
        |> List.distinct
        |> List.except bt

    let next1 =
        whiteNeighbors
        |> List.map (determineColor bt White)

    let next2 = bt |> List.map (determineColor bt Black)

    next1 @ next2
    |> List.filter (fun (tile, color) -> color = Black)
    |> List.map fst

let solve2 data =
    let blackTiles =
        data
        |> parseEachLine asCharArray
        |> Seq.map (Array.toList >> parseInstructions [])
        |> Seq.map toCoordinates
        |> Seq.countBy id
        |> Seq.filter (fun (_, count) -> count % 2 <> 0)
        |> Seq.toList
        |> List.map fst

    [ 1 .. 100 ]
    |> List.fold flip blackTiles
    |> List.length

[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/24/data.txt"
    Assert.Equal(3667, res)

[<Fact>]
let ``Solve 2 - example 1`` () =
    let res =
        solve2 "../../../Solutions/24/data-test-1.txt"

    Assert.Equal(2208, res)
