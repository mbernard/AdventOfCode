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
      Destinations: int64 list }

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
                      Destinations = [] }
                    { Source = xs.[9, *]
                      Side = Right
                      Destinations = [] }
                    { Source = xs.[*, 9]
                      Side = Bottom
                      Destinations = [] }
                    { Source = xs.[0, *]
                      Side = Left
                      Destinations = [] } ] })

    allTiles



let split lines =
    lines
    |> List.chunkBySize 12
    |> List.collect parseTile

let findMatches source tiles side =
    tiles
    |> List.filter (fun x ->
        let otherC =
            x.Connections
            |> List.find (fun c -> c.Side = side)

        otherC.Source = source)
    |> List.map (fun x -> x.Number)

let connectSide connection otherTiles =
    let otherSide =
        match connection.Side with
        | Top -> Bottom
        | Bottom -> Top
        | Right -> Left
        | Left -> Right

    findMatches connection.Source otherTiles otherSide

let findConnections tiles tile =
    let otherTiles =
        tiles
        |> List.filter (fun x -> x.Number <> tile.Number)

    let cs =
        tile.Connections
        |> List.map (fun x ->
            { x with
                  Destinations = connectSide x otherTiles })

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
            |> List.filter (fun c -> c.Destinations = List.empty)
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

let getSide cs side = cs |> List.find (fun x -> x.Side = side)

let findTile leftTile topTile candidate =
    let leftCondition =
        match leftTile with
        | None -> (getSide candidate.Connections Left).Destinations = List.empty
        | Some l ->
            (getSide candidate.Connections Left).Destinations |> List.contains l.Number
            && (getSide l.Connections Right).Destinations |> List.contains candidate.Number
            
    let topCondition =
        match topTile with
        | None -> (getSide candidate.Connections Top).Destinations = List.empty
        | Some t ->
            (getSide candidate.Connections Top).Destinations |> List.contains t.Number
            && (getSide t.Connections Bottom).Destinations |> List.contains candidate.Number
    
    leftCondition && topCondition

let placePiece (puzzle: Tile option [,]) tiles x y =
    let left =
        if x = 0 then None else puzzle.[x - 1, y]

    let top =
        if y = 0 then None else puzzle.[x, y - 1]

    let next = tiles |> List.find (findTile left top)
    puzzle.[x, y] <- Some next

    ()

let solvePuzzle tiles =
    let size =
        tiles
        |> List.length
        |> float
        |> (fun x -> x / 8.0)
        |> Math.Sqrt
        |> int

    let puzzle = Array2D.create size size None

    puzzle
    |> Array2D.iteri (fun x y _ -> placePiece puzzle tiles x y)
    
    puzzle
    |> Array2D.map (fun x -> trimBorders x.Value.Points)
    |> merge
    
let isMonster (sp:char [,]) (m:char[,]) =
    let mChars = m |> Seq.cast<char>
    let spChars = sp |> Seq.cast<char>
    Seq.forall2 (fun m sp -> m = ' ' || m = sp) mChars spChars
   
    
let findMonsters m p =
    let mutable count = 0
    for i in [0.. Array2D.length1 p - Array2D.length1 m] do
        for j in [0.. Array2D.length2 p - Array2D.length2 m] do
            let subPuzzle = p.[i..i + Array2D.length1 m - 1, j..j+Array2D.length2 m - 1]
            if isMonster subPuzzle m then count <- count+1
    count
    
let solve2 data =
    let tiles =
        data
        |> parseEachLine asString
        |> Seq.toList
        |> split
        |> connectTiles

    let puzzle = solvePuzzle tiles
    
    let monsterCharArray =
        "../../../Solutions/20/monster.txt"
        |> parseEachLine asCharArray
        |> Seq.toArray
    let monster = Array2D.init 3 20 (fun x y -> monsterCharArray.[x].[y])
    
    let ms =
        puzzle
        |> configuration
        |> List.map (findMonsters monster)
    let maxMonsters =
        ms
        |> List.max
    
    let rough = 
        puzzle
        |> Seq.cast<char>
        |> Seq.filter (fun x -> x = '#')
        |> Seq.length
    
    rough - maxMonsters * 15



[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/20/data.txt"
    Assert.Equal(2256, res)

[<Fact>]
let ``Solve 2 - example 1`` () =
    let res =
        solve2 "../../../Solutions/20/data-test-1.txt"

    Assert.Equal(273, res)
