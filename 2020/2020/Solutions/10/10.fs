module _10

open Xunit
open Common

let addOutlet xs = 0 :: xs

let addDevice xs =
    let device = xs |> List.max |> (+) 3
    device :: xs |> List.sort

let findDiff xs =
    xs
    |> List.pairwise
    |> List.map (fun (x, y) -> y - x)

let compute (xs: Map<int, int>) = xs.[1] * xs.[3]

let solve1 data =
    data
    |> parseEachLine asInt
    |> Seq.toList
    |> List.sort
    |> addOutlet
    |> addDevice
    |> findDiff
    |> List.countBy id
    |> Map.ofList
    |> compute

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/10/data.txt"
    Assert.Equal(2664, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/10/data-test-1.txt"

    Assert.Equal(35, res)

[<Fact>]
let ``Solve 1 - example 2`` () =
    let res =
        solve1 "../../../Solutions/10/data-test-2.txt"

    Assert.Equal(220, res)

let possibleNextItemsCount xs x =
    xs
    |> List.filter (fun y -> y > x && y <= x + 3)
    |> List.length

let compute2 (total, acc, count) x =
    match x with
    | 1 -> (total * acc, 1L, 0L)
    | 2 -> (total, 2L, 0L)
    | 3 ->
        match count with
        | 0L -> (total, 4L, 1L)
        | y -> (total, 3L * count + acc, y + 1L)
    | _ -> (total, acc, 0L)

let solve2 data =

    let xs =
        data
        |> parseEachLine asInt
        |> Seq.toList
        |> List.sort
        |> addOutlet
        |> addDevice

    xs
    |> List.map (possibleNextItemsCount xs)
    |> List.rev
    |> List.fold compute2 (1L, 1L, 0L)
    |> (fun (x, y, _) -> x * y)

[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/10/data.txt"
    Assert.Equal(148098383347712L, res)

[<Fact>]
let ``Solve 2 - example 1`` () =
    let res =
        solve2 "../../../Solutions/10/data-test-1.txt"

    Assert.Equal(8L, res)

[<Fact>]
let ``Solve 2 - example 2`` () =
    let res =
        solve2 "../../../Solutions/10/data-test-2.txt"

    Assert.Equal(19208L, res)
