module _10

open System
open Xunit
open Common

let getAngle opposite adjacent = Math.Atan(opposite / adjacent)
let getOppositeSide adjacent angle = Math.Tan(angle) * adjacent
let isInteger (f:float) = f % 1.0 = 0.0

let isCoord angle (x1,y1) (x:int) =
    let side = getOppositeSide (x |> float) angle
    if isInteger side then Some (x + x1, side |> int |> (+) y1) else None

let getPossibleObstacleCoord (x1:int, y1:int) (x2:int, y2:int) =
    if x1 = x2
    then 
        if y1 < y2 then [y1+1..y2-1] else [y1-1 .. -1 .. y2+1]
        |> List.map (fun y -> (x1, y))
    else if y1 = y2
    then 
        if x1 < x2 then [x1+1..x2-1] else [x1-1 .. -1 .. x2+1]
        |> List.map (fun x -> (x,y1))
    else
        let angle = getAngle (float (y2 - y1)) (float (x2 - x1))
        [1..abs(x2-x1-1)]
        |> List.choose (isCoord angle (x1,y1))
    |> Set.ofList

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
    then (c+1,xs,x)
    else (c,xs,x)

    //let obs =  
    //    getPossibleObstacleCoord x y
    //    |> Set.intersect xs
    //if Set.isEmpty obs 
    //then (c+1,xs,x)
    //else (c,xs,x)

let countDetection xs x =
    let (count, _, _) =
        Set.difference xs (Set.singleton x)
        |> Set.fold count (0,xs,x)
    (count, x)


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
        |> Seq.indexed
        |> Seq.fold getAsteroid Set.empty
    
    let counts = 
        asteriods
        |> Seq.map (countDetection asteriods)

    let (count, coord) = 
        counts
        |> Seq.maxBy fst
    Assert.Equal(expected, count)

[<Theory>]
[<InlineData(1,0,0,6,8)>]
[<InlineData(1,0,0,-6,-8)>]
[<InlineData(1,0,0,-6,8)>]
[<InlineData(1,0,0,6,-8)>]
[<InlineData(2,1,3,7,11)>]
[<InlineData(0,1,1,1,2)>]
[<InlineData(0,1,1,2,1)>]
[<InlineData(1,1,0,3,4)>]
let ``get obstacle`` expected x1 y1 x2 y2 =
    let obs = getPossibleObstacleCoord (x1,y1) (x2,y2)
    Assert.Equal(expected, obs |> Set.count)
