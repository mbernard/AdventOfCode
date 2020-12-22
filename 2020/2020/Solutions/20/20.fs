module _20

open System
open System.Text.RegularExpressions
open Xunit
open Common

type Tile =
    { Number: int64
      Points: char [,]
      Sides: char [] list }

let init (chars: char [] []) x y = chars.[x].[y]

let parseTile (x :: lines) =
    let number =
        Regex.Match(x, @"Tile (\d+):").Groups.[1].Value
        |> int64

    let chars =
        lines
        |> List.toArray
        |> (fun a -> Array.sub a 0 10)
        |> Array.map asCharArray

    let points = Array2D.init 10 10 (init chars)

    let sides =
        [ points.[*, 0]
          points.[*, 9]
          points.[0, *]
          points.[9, *] ]

    { Number = number
      Points = points
      Sides = sides }

let split lines =
    lines |> List.chunkBySize 12 |> List.map parseTile

let canConnectWith t1 t2 =

    t1.Sides
    |> List.exists (fun x1 ->
        t2.Sides |> List.contains x1
        || t2.Sides |> List.contains (x1 |> Array.rev))

let findConnections tiles tile =
    tiles
    |> List.except [ tile ]
    |> List.filter (canConnectWith tile)

let connectTiles tiles =
    tiles
    |> List.map (fun x -> x, findConnections tiles x)

let solve1 data =
    let tiles =
        data
        |> parseEachLine asString
        |> Seq.toList
        |> split
        |> connectTiles

    let corners =
        tiles
        |> List.filter (fun (t,ts) -> ts |> List.length = 2)
    corners
    |> List.map fst
    |> List.map (fun x -> x.Number)
    |> List.fold (*) 1L


[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/20/data.txt"
    Assert.Equal(23497974998093L, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/20/data-test-1.txt"

    Assert.Equal(20899048083289L, res)
