module _03

open Common
open System

type Direction = 
    | Left
    | Right
    | Up
    | Down

type Instruction = { Direction : Direction ; Number : int }

let nextPoint (x,y) number = 
    function
    | Up -> (x, y + number)
    | Down -> (x, y - number)
    | Right -> (x + number, y)
    | Left -> (x - number, y)

let trace (map:int[,], (x1,y1), intersect, lineId) instruction = 
    let (x2,y2) = nextPoint (x1, y1) instruction.Number instruction.Direction
    
    let mutable intersect2 = intersect
    for i in [Math.Min(x1, x2) .. Math.Max(x1, x2)] do
        for j in [Math.Min(y1, y2) .. Math.Max(y1, y2)] do
            if map.[i,j] <> -1 && map.[i,j] <> lineId then 
                intersect2 <- (i,j)::intersect
                map.[i,j] <- -2
            else
                map.[i,j] <- lineId

    (map, (x2, y2), intersect2, lineId)
    
let traceLine (map, startingPoint, intersections) (id, instructions) =
    let (map, _, i, _) =
        instructions
        |> Array.fold trace (map, startingPoint, intersections, id)

    (map, startingPoint, i)

let getIntersections map startingPoint lines =
    let (_, _, intersections) = lines |> Seq.fold traceLine (map, startingPoint, [])
    (startingPoint, intersections, map, lines)

let toPosition (x:string) = 
    let direction = 
        match x.Chars 0 with
        | 'U' -> Up
        | 'D' -> Down
        | 'L' -> Left
        | 'R' -> Right
        | e -> failwithf "Unable to parse direction %A" e
    { Direction = direction ; Number = x.Substring 1 |> int}
    

let parseLine id =
    splitBy "," (Array.map toPosition)
    >> (fun x -> (id, x))

let mDistance (x1:int, y1:int) (x2, y2) =
    Math.Abs(x2 - x1) + Math.Abs(y2 - y1) 

let distanceFromClosest (sp, intersections, _, _) =
    intersections
    |> List.except [sp]
    |> List.minBy (mDistance sp)
    |> mDistance sp

let solve path size =
    path
    |> parseEachLineIndexed parseLine
    |> getIntersections (Array2D.create size size -1) (size / 2, size /2)
    |> distanceFromClosest

let wireDistance (map:int[,], (x,y), steps, res) instruction  =
    let (x2,y2) = nextPoint (x, y) instruction.Number instruction.Direction
    let mutable mRes = res
    match instruction.Direction with
    | Up -> 
        for i in [0 .. instruction.Number] do
            if map.[x, y + i] = -2 then 
                map.[x, y + i] <- -3
                mRes <- (steps + i)::res
    | Down -> 
        for i in [0 .. instruction.Number] do
            if map.[x, y - i] = -2 then 
                map.[x, y - i] <- -3
                mRes <- (steps + i)::res
    | Right -> 
        for i in [0 .. instruction.Number] do
            if map.[x + i,y] = -2 then
                map.[x + i,y] <- -3
                mRes <- (steps + i)::res
    | Left -> 
        for i in [0 .. instruction.Number] do
            if map.[x-i,y] = -2 then 
                map.[x-i,y] <- -3
                mRes <- (steps + i)::res
    (map, (x2,y2), steps + instruction.Number, mRes)
    
let wiresDistance map sp (lineId, instructions) = 
    let _, _, _, x = instructions |> Seq.fold wireDistance (map, sp, 0, [])
    x

let getShorterWireLength (sp, intersections, map, lines) =
    let l1 = wiresDistance (Array2D.copy map) sp (Seq.item 0 lines) |> Seq.except [0]
    let l2 = wiresDistance map sp (Seq.item 1 lines) |> Seq.except [0]
    
    let sum = 
        Seq.allPairs l1 l2
        |> Seq.map (fun x -> fst x + snd x)
        |> Seq.sort
        |> Seq.toList

    sum
    |> Seq.min

let solve2 path size = 
    path
    |> parseEachLineIndexed parseLine
    |> getIntersections (Array2D.create size size -1) (size / 2, size /2)
    |> getShorterWireLength

open Xunit

[<Theory>]
[<InlineData(30,"../../../Data/03_test1.txt",20)>]
[<InlineData(610,"../../../Data/03_test2.txt",500)>]
[<InlineData(410,"../../../Data/03_test3.txt",500)>]
[<InlineData(0,"../../../Data/03.txt",30_000)>]
let ``solve 2`` expected path size =
    let actual = solve2 path size
    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(6,"../../../Data/03_test1.txt",20)>]
[<InlineData(166,"../../../Data/03_test2.txt",500)>]
[<InlineData(135,"../../../Data/03_test3.txt",500)>]
[<InlineData(293,"../../../Data/03.txt",30_000)>]
let ``solve 1`` expected path size =
    let actual = solve path size
    Assert.Equal(expected, actual)

[<Fact>]
let ``test toPosition`` () =
     let actual = toPosition "R994"
     let expected = { Direction = Right; Number = 994 }
     Assert.Equal(expected , actual)

