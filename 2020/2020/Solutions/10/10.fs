module _10

open Xunit
open Common

let addOutlet xs = 0::xs
let addDevice xs =
    let device =
        xs
        |> List.max
        |> (+) 3
    device::xs
    |> List.sort

let findDiff xs =
    xs
    |> List.pairwise
    |> List.map (fun (x,y) -> y-x)
    
let compute (xs: Map<int,int>) =
    xs.[1] * xs.[3]

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
let ``Solve 1``() =
    let res = solve1 "../../../Solutions/10/data.txt"
    Assert.Equal(2664, res)
    
[<Fact>]
let ``Solve 1 - example 1``() =
    let res = solve1 "../../../Solutions/10/data-test-1.txt"
    Assert.Equal(35, res)
    
[<Fact>]
let ``Solve 1 - example 2``() =
    let res = solve1 "../../../Solutions/10/data-test-2.txt"
    Assert.Equal(220, res)


