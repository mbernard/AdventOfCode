module _20
//
//open System
//open System.Text.RegularExpressions
//open Xunit
//open Common
//
//type Tile = { Number: int64; Points : char [,]; Sides : char [][] }
//
//let init (chars: char[][]) x y =
//    chars.[x].[y]
//    
//let parseTile (x::lines) =
//    let number = Regex.Match(x, @"Tile (\d+):").Groups.[1].Value |> int64
//    let chars =
//        lines
//        |> List.toArray
//        |> (fun a -> Array.sub a 0 10)
//        |> Array.map asCharArray
//        
//    let points = Array2D.init 10 10 (init chars)
//    let sides =
//        [ points.[*,0]
//        ]
//    {Number = number; Points = points}
//    
//let split lines =
//    lines
//    |> List.chunkBySize 12
//    |> List.map parseTile
//
//let solve1 data =
//    let d =
//        data
//        |> parseEachLine asString
//        |> Seq.toList
//        |> split
//    
//    0L
//
//
//[<Fact>]
//let ``Solve 1`` () =
//    let res = solve1 "../../../Solutions/20/data.txt"
//    Assert.Equal(0L, res)
//
//[<Fact>]
//let ``Solve 1 - example 1`` () =
//    let res =
//        solve1 "../../../Solutions/20/data-test-1.txt"
//
//    Assert.Equal(20899048083289L, res)