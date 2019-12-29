module _17

open Common
open System
open Xunit
open FSharpx.Collections

let parse = 
    "../../../Data/17.txt"
    |> parseFirstLine (splitBy "," asInt64Array)
   
let generateMap =
    let output =
        parse
        |> Computer.initialize64
        |> Computer.executeUntilHalt
        |> (fun x -> x.Output |> Queue.toSeq |> Seq.toArray)
        |> Array.map char
        |> charsToStr
        |> splitBy "\n" (Array.map Seq.toArray) 
    output

let getAt (x, y) = Array.tryItem y g |> Option.bind (Array.tryItem x) |> Option.defaultValue '.'

let isIntersection x y (map:int64[][]) =
    let n =
        [|  map.[x-1].[y]
            map.[x+1].[y]
            map.[x].[y-1]
            map.[x].[y+1]
        |]
    Array.TrueForAll(n, (fun z -> z <> 46L && z <> 10L))
    
let getIntersections (map:int64[][]) =
    let mutable sum = 0
    for i in [1..map.Length-3] do   
        for j in [1..map.[i].Length-2] do
            if isIntersection i j map 
            then sum <- sum + (i * j)
    sum

[<Fact>]
let ``solve 1`` () =
    let actual = 
        generateMap
        |> getIntersections
    Assert.Equal(0,actual)