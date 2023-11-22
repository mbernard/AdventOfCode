module day02

open System
open Xunit
open Common
open FsUnit

let measureSide (x,y) = x * y

let getSmallerSide sides =
    sides
    |> Array.sort
    |> fun xs -> (xs[0], xs[1])

let measureExtraPaper sides =
    sides
    |> getSmallerSide
    |> measureSide

let measurePaper sides =
    sides
    |> Array.pairwise
    |> Array.append [|(sides[0], sides[2])|]
    |> Array.map measureSide
    |> Array.sum
    |> (*) 2
    |> (+) (sides |> measureExtraPaper)

[<Fact>]
let solve1 () = 
    "../../../Solutions/02/data.txt"
    |> parseEachLine extractInts
    |> Seq.map measurePaper
    |> Seq.sum
    |> should equal 1606483

let measureRibbon sides =
    sides
    |> getSmallerSide
    |> (fun (x,y) -> x * 2 + y * 2)
    |> (+) (sides |> Array.fold (*) 1)

[<Fact>]
let solve2 () = 
    "../../../Solutions/02/data.txt"
    |> parseEachLine extractInts
    |> Seq.map measureRibbon
    |> Seq.sum
    |> should equal 3842356