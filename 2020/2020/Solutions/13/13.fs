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
        let busID, time =
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

let rec diff acc busId x =
    if acc - x >= 0L
    then acc - x
    else diff (acc + busId) busId x 

let toBusRestrictions (data: string) =
    data.Split(',')
    |> Array.mapi (fun i x ->
        match x with
        | "x" -> (i |> int64,None)
        | y -> i |> int64, y |> int64 |> Some)
    |> Array.filter (fun (x,y) -> Option.isSome y)
    |> Array.map (fun (x,y) ->
        let busId = Option.get y
        
        let diff = diff busId busId x
        (diff, busId))
  
let rec sieve (cs: (int64 * int64) list) (x:int64) (N:int64) =
    match cs with
    | [] -> Some(x)
    | (a,n)::rest ->
        let arrProgress = Seq.unfold (fun x -> Some(x, x+N)) x
        let firstXmodNequalA = Seq.tryFind (fun x -> a = x % n)
        match firstXmodNequalA (Seq.take (n |> int) arrProgress) with
        | None -> None
        | Some(x) -> sieve rest x (N*n)
 


let solve2 data =
    let cs =
        data
        |> parseEachLine asString
        |> Seq.toArray
        |> (fun x -> x.[1])
        |> toBusRestrictions
        |> Array.toList
        |> List.map (fun (a,n) -> (a % n, n))
        |> List.sortBy (snd>>(~-)) 
    let (a,n)::rest = cs
    
    sieve rest a n
    |> Option.get
    

[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/13/data.txt"
    Assert.Equal(626670513163231L, res)

[<Fact>]
let ``Solve 2 - example 1`` () =
    let res =
        solve2 "../../../Solutions/13/data-test-1.txt"

    Assert.Equal(1068781L, res)
    
[<Fact>]
let ``Solve 2 - example 2`` () =
    let res =
        solve2 "../../../Solutions/13/data-test-2.txt"

    Assert.Equal(3417L, res)
