﻿module _11

open Common
open Xunit
open Computer
open FSharpx.Collections

type Direction =
    | Up
    | Down
    | Right
    | Left

type Square = { Position: int * int ; Color : int }
type Robot = { Direction: Direction; Position: int * int; Path: Square list; Computer: Computer }

let readColor r =
    List.tryFindBack (fun (x:Square) -> x.Position = r.Position) r.Path
    |> Option.defaultValue { Position = r.Position; Color = 0 }
    |> (fun x -> x.Color)

let turnLeft r = 
    match r.Direction with
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

let turnRight r =
    match r.Direction with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let turn x r =
    let d = if x = 0 then turnLeft else turnRight
    { r with Direction = d r}

let moveForward r = 
    let x,y = r.Position
    let pos =
        match r.Direction with
        | Up -> (x, y + 1)
        | Down -> (x, y - 1)
        | Right -> (x + 1, y)
        | Left -> (x - 1, y)
    { r with Position = pos; }

let computeNextColor robot =
    let color = robot |> readColor |> int64
    let c = 
        robot.Computer
        |> writeInput color
        |> executeUntilOutput
    { robot with Computer = c }

let paint robot =
    let color, c =
        robot.Computer
        |> readOutput
    let p =
        robot.Path
        |> List.cons { Position = robot.Position ; Color = color |> int }
    { robot with Computer = c ; Path = p }

let move robot =
    let direction, c = 
        robot.Computer
        |> executeUntilOutput
        |> readOutput
    let r = 
        robot
        |> turn (direction |> int) 
        |> moveForward
    { r with Computer = c }

let rec execute robot =
    let r = robot |> computeNextColor
    match r.Computer.State with
    | Done -> r
    | Executing -> 
        paint r
        |> move
        |> execute

let solve program =
    { Direction = Up; Position = 0,0 ; Path = List.empty; Computer = initialize64 program Queue.empty }
    |> execute

[<Fact>]
let ``solve 1`` () =
    let r =
        "../../../Data/11.txt"
        |> parseFirstLine (splitBy "," asInt64Array)
        |> solve
    let actual =
        r.Path
        |> List.distinctBy (fun x -> x.Position)
        |> List.length
    Assert.Equal(0, actual)