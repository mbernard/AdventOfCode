module _20

open System
open System.Text.RegularExpressions
open Xunit
open Common


type Side =
    | Top
    | Right
    | Left
    | Bottom

type Connection =
    { Source: char []
      Side: Side
      Destination: int64 option }

type Tile =
    { Number: int64
      Points: char [,]
      Connections: Connection list }

type Position =
    | Corner
    | Border
    | Middle

let rotate90 m =
    [ for col in (Array2D.length1 m) - 1 .. -1 .. 0 -> m.[0.., col] ]
    |> array2D

let flip m =
    [ for row in (Array2D.length1 m) - 1 .. -1 .. 0 -> m.[row, 0..] ]
    |> array2D

let trimBorders (m: 'a [,]) =
    m.[1..(Array2D.length1 m - 2), 1..(Array2D.length2 m - 2)]

let merge (nested: char [,] [,]) =
    let outerDim = Array2D.length1 nested
    let innerDim = Array2D.length1 nested.[0, 0]

    [ for outer in 0 .. outerDim - 1 do
        for inner in 0 .. innerDim - 1 ->
            nested.[outer, 0..]
            |> Seq.collect (fun m -> m.[inner, 0..])
            |> Seq.toList ]
    |> array2D

let configuration tile =
    [ tile
      tile |> rotate90
      tile |> rotate90 |> rotate90
      tile |> rotate90 |> rotate90 |> rotate90 ]
    |> List.collect (fun t -> [ t; flip t ])

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

    let allTiles =
        configuration points
        |> List.distinct
        |> List.map (fun xs ->
            { Number = number
              Points = xs
              Connections =
                  [ { Source = xs.[*, 0]
                      Side = Top
                      Destination = None }
                    { Source = xs.[9, *]
                      Side = Right
                      Destination = None }
                    { Source = xs.[*, 9]
                      Side = Bottom
                      Destination = None }
                    { Source = xs.[0, *]
                      Side = Left
                      Destination = None } ] })

    allTiles



let split lines =
    lines
    |> List.chunkBySize 12
    |> List.collect parseTile

let findMatch source tiles side =
    tiles
    |> List.filter (fun x ->
        let otherC =
            x.Connections
            |> List.find (fun c -> c.Side = side)

        otherC.Source = source)
    |> List.map (fun x -> x.Number)
    |> List.tryExactlyOne

let connectSide connection otherTiles =
    let otherSide =
        match connection.Side with
        | Top -> Bottom
        | Bottom -> Top
        | Right -> Left
        | Left -> Right

    findMatch connection.Source otherTiles otherSide

let findConnections tiles tile =
    let otherTiles =
        tiles
        |> List.filter (fun x -> x.Number <> tile.Number)

    let cs =
        tile.Connections
        |> List.map (fun x ->
            { x with
                  Destination = connectSide x otherTiles })

    { tile with Connections = cs }

let getPosition xs =
    match List.length xs with
    | 2 -> Corner
    | 3 -> Border
    | _ -> Middle

let connectTiles tiles =
    tiles |> List.map (findConnections tiles)

let solve1 data =
    let tiles =
        data
        |> parseEachLine asString
        |> Seq.toList
        |> split
        |> connectTiles

    let corners =
        tiles
        |> List.filter (fun x ->
            x.Connections
            |> List.filter (fun c -> c.Destination = None)
            |> List.length = 2)

    corners
    |> List.map (fun x -> x.Number)
    |> List.distinct
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

let isCorner xs = xs |> List.length = 2
let isBorder xs = xs |> List.length = 3
//
//let rec placePiece p x :: rest =
//
//    placePiece p rest
//
//let solvePuzzle tiles =
//    let c :: corners =
//        tiles
//        |> List.filter (fun x -> x.Position = Corner)
//
//    let borders =
//        tiles
//        |> List.filter (fun x -> x.Position = Border)
//
//    let size =
//        tiles |> List.length |> float |> Math.Sqrt |> int
//
//    let puzzle = Array2D.create size size None
//
//    tiles |> List.zip tiles |> Map.ofList
//
//    buildBorder c p
//
////    placePiece puzzle |> Array2D.map Option.get
//
//let solve2 data =
//    let tiles =
//        data
//        |> parseEachLine asString
//        |> Seq.toList
//        |> split
//        |> connectTiles
//
//    tiles |> solvePuzzle
//
//
//[<Fact>]
//let ``Solve 2`` () =
//    let res = solve1 "../../../Solutions/20/data.txt"
//    Assert.Equal(0, res)
//
//[<Fact>]
//let ``Solve 2 - example 1`` () =
//    let res =
//        solve1 "../../../Solutions/20/data-test-1.txt"
//
//    Assert.Equal(273, res)
