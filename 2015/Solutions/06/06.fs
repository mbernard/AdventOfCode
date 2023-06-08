module day06

open System
open Xunit
open Common
open FsUnit
open Microsoft.FSharp.Math

type Action =
    | TurnOn
    | TurnOff
    | Toggle

type Point = { X: int; Y: int }

type Instruction =
    { From: Point
      To: Point
      Action: Action }

let parseAction =
    function
    | "turn on" -> TurnOn
    | "turn off" -> TurnOff
    | "toggle" -> Toggle
    | _ -> failwith "Unknown action"

let toInstruction (xs: string array) =
    { From = { X = int xs[1]; Y = int xs[2] }
      To = { X = int xs[3]; Y = int xs[4] }
      Action = parseAction xs[0] }

let applyInstructions (map: int [,]) x =
    for i in x.From.X .. x.To.X do
        for j in x.From.Y .. x.To.Y do
            map[i, j] <- match x.Action with
                         | TurnOn -> 1
                         | TurnOff -> 0
                         | Toggle -> if map[i, j] = 1 then 0 else 1

    map

[<Fact>]
let solve1 () =
    "../../../Solutions/06/data.txt"
    |> parseEachLine (withRegex "(.*) (\d+),(\d+) through (\d+),(\d+)")
    |> Seq.map toInstruction
    |> Seq.fold applyInstructions (Array2D.create 1000 1000 0)
    |> Seq.cast<int>
    |> Seq.sum
    |> should equal 400410

let applyInstructions2 (map: int [,]) x =
    for i in x.From.X .. x.To.X do
        for j in x.From.Y .. x.To.Y do
            map[i, j] <- match x.Action with
                         | TurnOn -> map[i, j] + 1
                         | TurnOff -> if map[i, j] = 0 then 0 else map[i, j] - 1
                         | Toggle -> map[i, j] + 2

    map

[<Fact>]
let solve2 () =
    "../../../Solutions/06/data.txt"
    |> parseEachLine (withRegex "(.*) (\d+),(\d+) through (\d+),(\d+)")
    |> Seq.map toInstruction
    |> Seq.fold applyInstructions2 (Array2D.create 1000 1000 0)
    |> Seq.cast<int>
    |> Seq.sum
    |> should equal 15343601