module _09

open System.Collections
open Xunit
open Common

let isValid x = Array.exists (fun (y,z) -> y+z = x)

let rec findInvalid i size (data:int64[]) =
    let w = Array.sub data i size
    let pairs = Array.allPairs w w
    if isValid data.[i+size] pairs
    then findInvalid (i+1) size data
    else data.[i+size]
    
let rec findRange i size data invalid =
    let s =
        Array.sub data i size
        |> Array.sum
    match s with
    | s when s = invalid -> (i,size)
    | s when s < invalid -> findRange i (size + 1) data invalid
    | s when s > invalid -> findRange (i+1) 2 data invalid
    | _ -> failwith "no match found"
    
let solveRange (data:int64[]) (i,size) =
    let range = Array.sub data i size
    Array.min range + Array.max range

let solve2 data =
    let d = 
        data
        |> parseEachLine asInt64
        |> Seq.toArray
    
    d
    |> findInvalid 0 25
    |> findRange 0 2 d
    |> solveRange d

[<Fact>]
let ``Solve 2 - example 1`` () =
    let d =
        "../../../Solutions/09/data-test-1.txt"
        |> parseEachLine asInt64
        |> Seq.toArray

    let res =
        d
        |> findInvalid 0 5
        |> findRange 0 2 d
        |> solveRange d
   
    Assert.Equal(62L, res)
        
[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/09/data.txt"
    Assert.Equal(31580383L, res)
    
let solve1 data =
    data
    |> parseEachLine asInt64
    |> Seq.toArray
    |> findInvalid 0 25

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        "../../../Solutions/09/data-test-1.txt"
        |> parseEachLine asInt64
        |> Seq.toArray
        |> findInvalid 0 5
    Assert.Equal(127L, res)
    
[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/09/data.txt"
    Assert.Equal(248131121L, res)

