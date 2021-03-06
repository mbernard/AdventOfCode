﻿module _17

open System
open System.Text.RegularExpressions
open Xunit
open Common

type State =
    | Active
    | Inactive

type Cell =
    { State: State
      Position: int * int * int }

let initLoad y (line: char []) =
    line
    |> Array.mapi (fun x c ->
        { State = if c = '#' then Active else Inactive
          Position = (x, y, 0) })
    |> Array.toList

let isNeighborOf (cx, cy, cz) n =
    let (x, y, z) = n.Position

    cx - 1 <= x
    && x <= cx + 1
    && cy - 1 <= y
    && y <= cy + 1
    && cz - 1 <= z
    && z <= cz + 1


let findNeighbors cells c =
    cells
    |> List.filter (isNeighborOf c.Position)
    |> List.except [ c ]

let computeState cells c =
    let neighbors = findNeighbors cells c

    let activeCount =
        neighbors
        |> List.filter (fun x -> x.State = Active)
        |> List.length

    let newState =
        match c.State with
        | Active -> if activeCount = 2 || activeCount = 3 then Active else Inactive
        | Inactive -> if activeCount = 3 then Active else Inactive

    { c with State = newState }

let extend (cells: Cell list) =
    let positions = cells |> List.map (fun c -> c.Position)

    let minX =
        positions
        |> List.map (fun (x, _, _) -> x)
        |> List.min

    let maxX =
        positions
        |> List.map (fun (x, _, _) -> x)
        |> List.max

    let minY =
        positions
        |> List.map (fun (_, y, _) -> y)
        |> List.min

    let maxY =
        positions
        |> List.map (fun (_, y, _) -> y)
        |> List.max

    let minZ =
        positions
        |> List.map (fun (_, _, z) -> z)
        |> List.min

    let maxZ =
        positions
        |> List.map (fun (_, _, z) -> z)
        |> List.max

    let r =
        seq {
            for x in [ minX - 1 .. maxX + 1 ] do
                for y in [ minY - 1 .. maxY + 1 ] do
                    for z in [ minZ - 1 .. maxZ + 1 ] do
                        yield
                            cells
                            |> List.tryFind (fun c -> c.Position = (x, y, z))
                            |> function
                            | Some c -> c
                            | None ->
                                { State = Inactive
                                  Position = (x, y, z) }
        }
        |> Seq.toList

    r

let boot cells cyclesLeft =
    cells |> extend |> List.map (computeState cells)

let solve1 data =
    let cells =
        data
        |> parseEachLine asCharArray
        |> Seq.mapi initLoad
        |> Seq.toList
        |> Seq.fold (fun s x -> x @ s) []

    [ 1 .. 6 ]
    |> List.fold boot cells
    |> List.filter (fun x -> x.State = Active)
    |> List.length


[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/17/data.txt"
    Assert.Equal(252, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/17/data-test-1.txt"

    Assert.Equal(112, res)
