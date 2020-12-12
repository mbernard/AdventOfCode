module _12

open FSharpx.Collections
open Xunit
open Common

type Direction =
    | N
    | E
    | W
    | S

type Instruction =
    | Direction of Direction
    | F
    | L
    | R


let parse (xs: string array) =
    let i =
        match xs.[0] with
        | "F" -> F
        | "L" -> L
        | "R" -> R
        | "N" -> Direction N
        | "S" -> Direction S
        | "E" -> Direction E
        | "W" -> Direction W
        | _ -> failwith "fail to parse value"

    (i, xs.[1] |> int)

let rec turnLeft d degrees =
    if degrees = 0 then
        d
    else
        let nd =
            match d with
            | N -> W
            | W -> S
            | S -> E
            | E -> N

        turnLeft nd (degrees - 90)

let rec turnRight d degrees =
    if degrees = 0 then
        d
    else
        let nd =
            match d with
            | N -> E
            | E -> S
            | S -> W
            | W -> N

        turnRight nd (degrees - 90)

let nextPosition (ns, ew) facing v =
    match facing with
    | N -> ns + v, ew
    | S -> ns - v, ew
    | E -> ns, ew + v
    | W -> ns, ew - v


let move (ns, ew, facing) (instruction, v) =
    match instruction with
    | F ->
        let x, y = nextPosition (ns, ew) facing v
        (x, y, facing)
    | L -> ns, ew, turnLeft facing v
    | R -> ns, ew, turnRight facing v
    | Direction x ->
        let x, y = nextPosition (ns, ew) x v
        (x, y, facing)

let solve1 data =
    data
    |> parseEachLine (withRegex @"([A-Z])(\d+)")
    |> Seq.map parse
    |> Seq.fold move (0, 0, E)
    |> (fun (x, y, _) -> abs x + abs y)

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/12/data.txt"
    Assert.Equal(1221, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/12/data-test-1.txt"

    Assert.Equal(25, res)
