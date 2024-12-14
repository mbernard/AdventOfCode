module day02

open System
open Xunit
open Common
open FsUnit

type ReportState = Safe | Unsafe
type Direction = Increase | Decrease
let getReportState (report: int list) =
    let first::second::rest = report
    let direction = if second - first > 0 then Increase else Decrease
    let rec loop prev xs =
        match xs with
        | [] -> Safe
        | x :: rest -> 
            let diff = x - prev
            let localDirection = if diff > 0 then Increase else Decrease
            if localDirection = direction 
            then 
                if abs diff >= 1 && abs diff <= 3 then loop x rest
                else Unsafe
            else Unsafe
    loop first (second::rest)

let solve1 input =
    input
    |> Seq.map getReportState
    |> Seq.filter (fun x -> x = Safe)
    |> Seq.length

let parse path = 
    path
    |> parseEachLine (splitBy " " asIntArray >> Array.toList)

let testData = 
        [
            [7; 6; 4; 2; 1]
            [1; 2; 7; 8; 9]
            [9; 7; 6; 2; 1]
            [1; 3; 2; 4; 5]
            [8; 6; 4; 4; 1]
            [1; 3; 6; 7; 9]
        ] 

[<Fact>]
let solve1_testdata () =
    solve1 testData
    |> should equal 2

[<Fact>]
let solve1_test () =
    parse "../../../Solutions/02/data.txt"
    |> solve1
    |> should equal 502

let solve2 input =
    input
    |> Seq.map (fun x ->
        match getReportState x with
        | Safe -> Safe
        | Unsafe -> 
            let gen i =
                if i = List.length x
                then None
                else
                    (List.removeAt i x, i+1) |> Some
            List.unfold gen 0
            |> List.map getReportState
            |> List.tryFind (fun x -> x = Safe)
            |> Option.defaultValue Unsafe
        )
    |> Seq.filter (fun x -> x = Safe)
    |> Seq.length

[<Fact>]
let solve2_testdata () =
    solve2 testData
    |> should equal 4

[<Fact>]
let solve2_test () =
    parse "../../../Solutions/02/data.txt"
    |> solve2
    |> should equal 544