module _10

open System
open Xunit
open Common

let getAngle opposite adjacent = Math.Atan(opposite / adjacent)
let getOppositeSide adjacent angle = Math.Tan(angle) * adjacent
let isInteger (f:float) = f % 1.0 = 0.0

let getAsteriodOnLine (s,i) (j,c) =
    if c = '#' 
    then (Set.add (j,i) s, i)
    else (s, i)

let getAsteroid s (i,x:string) =
    x.ToCharArray()
    |> Array.indexed
    |> Array.fold getAsteriodOnLine (s,i)
    |> fst

let getAngleForPoints (x1,y1) (x2,y2) =
    getAngle (float (y2 - y1)) (float (x2 - x1))

let areFloatKindaEqual x y = abs(x - y) < 0.00001

let inBoundaries (x1,y1) (x2,y2) (x3,y3) =
    let minX = min x1 x2
    let maxX = max x1 x2
    let minY = min y1 y2
    let maxY = max y1 y2
    x3 >= minX && x3 <= maxX && y3 >= minY && y3 <= maxY

let count (c,xs,x) y =
    let angle = getAngleForPoints x y
    let s = xs |> Set.filter (inBoundaries x y) |> Set.filter (fun e -> e <> x && e <> y)
    if s |> Set.forall (fun e -> areFloatKindaEqual (getAngleForPoints x e) angle |> not)
    then (y::c,xs,x)
    else (c,xs,x)

let countDetection xs x =
    let (detections, _, _) =
        Set.difference xs (Set.singleton x)
        |> Set.fold count ([],xs,x)
    (detections, x)

let getQuadrant point =
    match point with
    | (x,y) when x >= 0 && y > 0 -> 0
    | (x,y) when x > 0 && y <= 0 -> 1
    | (x,y) when x <= 0 && y < 0 -> 2
    | (x,y) when x < 0 && y >= 0 -> 3
    | _ -> failwith "unkown coord quadrant"

let getFullAngle station x =
    let q = getQuadrant x * 90 |> float
    let a = Math.PI * getAngleForPoints station x / 180.0
    (a + q, x)

[<Theory>]
[<InlineData(0,0,1)>]
[<InlineData(0,1,1)>]
[<InlineData(1,1,0)>]
[<InlineData(1,1,-1)>]
[<InlineData(2,0,-1)>]
[<InlineData(2,-1,-1)>]
[<InlineData(3,-1,0)>]
[<InlineData(3,-1,1)>]
let ``get quadrant test`` expected x y =
    let actual = getQuadrant (x,y)
    Assert.Equal(expected, actual)

let testCases: obj array seq =
    seq {
        yield [| 8; "../../../Data/10_test1.txt" |]
        yield [| 33; "../../../Data/10_test2.txt" |]
        yield [| 35; "../../../Data/10_test3.txt" |]
        yield [| 41; "../../../Data/10_test4.txt" |]
        yield [| 210; "../../../Data/10_test5.txt" |]
        yield [| 347; "../../../Data/10.txt" |]
        }


[<Theory>]
[<MemberData("testCases")>]
let ``solve 1`` expected file =
    let asteriods = 
        parseEachLine asString file
        |> Seq.toArray
        |> Array.indexed
        |> Array.fold getAsteroid Set.empty
    
    let counts = 
        asteriods
        |> Set.map (countDetection asteriods)

    let count = 
        counts
        |> Seq.maxBy (fst >> List.length)
        |> (fst >> List.length)
    Assert.Equal(expected, count)

let testCases2: obj array seq =
    seq {
        yield [| 0; "../../../Data/10.txt" |]
        }

[<Theory>]
[<MemberData("testCases2")>]
let ``solve 2`` expected file =
    let asteriods = 
        parseEachLine asString file
        |> Seq.toArray
        |> Array.indexed
        |> Array.fold getAsteroid Set.empty
    
    let (detections, station) = 
        asteriods
        |> Set.map (countDetection asteriods)
        |> Seq.maxBy (fst >> List.length)
    let actual = 
        detections
        |> List.map (getFullAngle station)
        |> List.item 199
        |> snd
        |> (fun (x,y) -> x * 100 + y)
   
    Assert.Equal(expected, actual)
