module _13

open FSharpx.Collections
open Xunit
open Common
open System

type Data =
    { EarliestDeparture: int
      BusSchedule: int [] }

let parse (xs: string []) =
    let bus =
        xs.[1].Split(',')
        |> Array.filter (fun x -> x <> "x")
        |> Array.map int

    { EarliestDeparture = xs.[0] |> int
      BusSchedule = bus }

let rec computeDeparture time bus =
    bus
    |> Array.tryFind (fun x -> time % x = 0)
    |> function
    | None -> computeDeparture (time + 1) bus
    | Some bus -> (bus, time) 

let solve1 data =
    data
    |> parseEachLine asString
    |> Seq.toArray
    |> parse
    |> (fun x ->
        let busID,time =
            computeDeparture x.EarliestDeparture x.BusSchedule

        (time - x.EarliestDeparture) * busID)

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/13/data.txt"
    Assert.Equal(6559, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/13/data-test-1.txt"

    Assert.Equal(295, res)

//let solve2 data =
//    data
//    |> parseEachLine (withRegex @"([A-Z])(\d+)")
//    |> Seq.map parse
//    |> Seq.fold execute ({ EW = 0; NS = 0 }, { EW = 10; NS = 1 })
//    |> fst
//    |> (fun ship -> abs ship.EW + abs ship.NS)
//
//
//[<Fact>]
//let ``Solve 2`` () =
//    let res = solve2 "../../../Solutions/13/data.txt"
//    Assert.Equal(59435, res)
//
//[<Fact>]
//let ``Solve 2 - example 1`` () =
//    let res =
//        solve2 "../../../Solutions/13/data-test-1.txt"
//
//    Assert.Equal(286, res)
