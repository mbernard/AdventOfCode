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

let execute (c, r) ins =
    let isEvenRow = r % 2 = 0
    match ins with
    | E -> (c+1,r)
    | W -> (c-1,r)
    | NE -> if isEvenRow then (c+1,r+1) else (c,r+1)
    | NW -> if isEvenRow then (c,r+1) else (c-1,r+1)
    | SE -> if isEvenRow then (c+1,r-1) else (c,r-1)
    | SW -> if isEvenRow then (c,r-1) else (c-1,r-1)     
    
let toCoordinates xs =
    xs
    |> List.fold execute (0,0)

let solve1 data =
    data
    |> parseEachLine asCharArray
    |> Seq.map (Array.toList >> parseInstructions [])
    |> Seq.map toCoordinates
    |> Seq.countBy id
    |> Seq.filter (fun ((x,y),count) -> count % 2 <> 0)
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
