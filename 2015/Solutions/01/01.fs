module day01

open System
open Xunit
open Common
open FsUnit

let countFloor s x =
    match x with
    | '(' -> s + 1
    | ')' -> s - 1
    | _ -> failwith "invalid char"

let findBasementIndex s x =
    let (i, floor) = s
    let (xi, c) = x

    if floor < 0 then
        s
    else
        let newX = countFloor floor c

        if newX < 0 then
            (xi, newX)
        else
            (0, newX)

[<Fact>]
let solve1 () =
    "../../../Solutions/01/data.txt"
    |> parseFirstLine asCharArray
    |> Array.fold countFloor 0
    |> should equal 280





[<Fact>]
let solve2 () =
    "../../../Solutions/01/data.txt"
    |> parseFirstLine asCharArray
    |> Array.indexed
    |> Array.fold findBasementIndex (0, 0)
    |> fst
    |> (+) 1
    |> should equal 1797
