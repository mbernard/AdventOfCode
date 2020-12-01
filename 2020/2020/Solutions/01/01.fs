module _01

open Common
open Xunit

let rec getResult total i (xs: int []) =
    if i >= Array.length xs then
        None
    else
        xs
        |> Array.tryFind (fun x -> x = total - xs.[i])
        |> function
        | None -> getResult total (i + 1) xs
        | Some x -> (xs.[i], x) |> Some

let rec getResult2 i (xs: int []) =
    let rest = 2020 - xs.[i]

    xs
    |> getResult rest 0
    |> function
    | None -> getResult2 (i + 1) xs
    | Some (x, y) -> xs.[i], x, y

let solve1 x =
    x
    |> parseEachLine asInt
    |> Seq.toArray
    |> getResult 2020 0
    |> Option.get
    |> fun (x, y) -> x * y

let solve2 x =
    x
    |> parseEachLine asInt
    |> Seq.toArray
    |> getResult2 0
    |> fun (x, y, z) -> x * y * z
    
[<Fact>]
let ``Example 1 - solve 2`` () =
    let res =
        solve2 "../../../Solutions/01/data-test-1.txt"

    Assert.Equal(241861950, res)

[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/01/data.txt"
    Assert.Equal(165080960, res)

[<Fact>]
let ``Example 1`` () =
    let res =
        solve1 "../../../Solutions/01/data-test-1.txt"

    Assert.Equal(514579, res)

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/01/data.txt"
    Assert.Equal(157059, res)
