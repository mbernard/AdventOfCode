module _01

open Common
open Xunit

let rec findRes i (xs: int []) =
    xs
    |> Array.tryFind (fun x -> x = 2020 - xs.[i])
    |> function
    | None -> findRes (i + 1) xs
    | Some x -> xs.[i] * x

let solve1 x =
    x
    |> parseEachLine asInt
    |> Seq.toArray
    |> findRes 0

//let solve2 =
//    "data.txt"
//    |> parseEachLine asInt
//    |> Seq.map getFuel2
//    |> Seq.reduce (+)
//    |> printf "%i"

[<Fact>]
let ``Example 1`` () =
    let res = solve1 "../../../Solutions/01/data-test-1.txt"
    Assert.Equal(514579, res)
    
[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/01/data.txt"
    Assert.Equal(157059, res)
