module day14

open System
open Xunit
open Common
open FsUnit

[<Measure>] type km
[<Measure>] type s

type Reindeer = 
    {
        name: string
        speed: int<km/s>
        endurance: int<s>
        recovery: int<s>
    }

let toReindeer (xs: string array) =
    {
        name = xs[0]
        speed = int xs[1] * 1<km/s>
        endurance = int xs[2] * 1<s>
        recovery = int xs[3] * 1<s>
    }

let distanceAtTime s rd =
    let totalCycleTime = rd.endurance + rd.recovery
    let numberOfCycles = s / totalCycleTime
    let remainder = s % totalCycleTime
    let remainderEndurance = if remainder > rd.endurance then rd.endurance else remainder
    let distance = (numberOfCycles * rd.endurance * rd.speed) + (remainderEndurance * rd.speed)

    (rd, distance)

[<Theory>]
[<InlineData(2655, "../../../Solutions/14/data.txt")>]
let solve1 expected input =
    input
    |> parseEachLine (withRegex "^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.$")
    |> Seq.map toReindeer
    |> Seq.map (distanceAtTime 2503<s>)
    |> Seq.maxBy snd
    |> snd
    |> should equal expected

let scoreDeer (s:Map<Reindeer,int>) (x:Reindeer) =
    s |> Map.change x (fun oldValue -> oldValue.Value + 1 |> Some)

let score scores  time =
    let deerDistanceAtTime =
        scores
        |> Map.keys
        |> Seq.map (distanceAtTime (time * 1<s>))

    let maxDistance = deerDistanceAtTime |> Seq.maxBy snd |> snd
   
    deerDistanceAtTime
    |> Seq.filter (fun (deer,distance) -> distance = maxDistance)
    |> Seq.map fst
    |> Seq.fold scoreDeer scores
    
    

[<Theory>]
[<InlineData(1059, "../../../Solutions/14/data.txt")>]
let solve2 expected input =
    let scores =
        input
        |> parseEachLine (withRegex "^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.$")
        |> Seq.map toReindeer
        |> Seq.map (fun x -> (x, 0))
        |> Map.ofSeq
    
    [1..2503]
    |> Seq.fold score scores
    |> Map.toSeq
    |> Seq.maxBy snd
    |> snd
    |> should equal expected
