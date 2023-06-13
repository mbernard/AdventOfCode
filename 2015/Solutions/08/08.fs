module day08

open System
open Xunit
open Common
open FsUnit

let countCodeChar (xs:char array) =
    let mutable i = 1;
    let mutable count = 2
    let max = xs.Length-1 
    while i < max do
        if xs[i] = '\\' then
            if i + 1 < max then
                match xs[i+1] with
                | '\\'
                | '"' -> 
                    count <- count + 1
                    i <- i + 1
                | 'x' ->
                    count <- count + 3
                    i <- i + 3
        i <- i + 1
    count

[<Theory>]
[<InlineData(2, @"""""")>]
[<InlineData(2, @"""abc""")>]
[<InlineData(3, "\"aaa\\\"aaa\"")>]
[<InlineData(5, "\"\\x27\"")>]
let testCountCodeChar (expected:int) (x:string) =
    countCodeChar (asCharArray x)
    |> should equal expected

[<Fact>]
let solve1 () =
    "../../../Solutions/08/data.txt"
    |> parseEachLine asCharArray
    |> Seq.sumBy countCodeChar
    |> should equal 1350

let encodeChar x =
    match x with
    | '"' -> [|'\\'; '"'|]
    | '\\' -> [|'\\'; '\\'|]
    | _ -> [|x|]

let encode xs =
    let encoded = xs |> Array.map encodeChar |> Array.concat
    let c = [|'"'|]
    [|c; encoded; c|]
    |> Array.concat

[<Fact>]
let solve2 () = 
    "../../../Solutions/08/data.txt"
    |> parseEachLine asCharArray
    |> Seq.map encode
    |> Seq.sumBy countCodeChar
    |> should equal 2085

  