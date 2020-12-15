module _15

open System
open Xunit
open Common

type Game =
    { Turn: int
      LastSpoken: int
      Memory: Map<int, int * int> }

let load s x =
    { Turn = s.Turn + 1
      LastSpoken = x
      Memory = Map.add x (s.Turn,0) s.Memory }

let init (x: string) =
    x.Split(',')
    |> Array.map int
    |> Array.fold load { Turn = 1; LastSpoken = 0; Memory = Map.empty }
    
let rec play stop game =
    let (last, last') = game.Memory |> Map.find game.LastSpoken
    let next =
        if last' = 0 then 0
        else last - last'
   
    if stop = game.Turn then next
    else
        let last'' =
            match game.Memory |> Map.tryFind next with
            | None -> 0
            | Some x -> fst x 
        play stop { Turn = game.Turn + 1
                    LastSpoken = next
                    Memory = Map.add next (game.Turn, last'') game.Memory }

let solve1 data =
    data
    |> init
    |> play 2020

let solve2 data =
    data
    |> init
    |> play 30000000

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "2,0,6,12,1,3"
    Assert.Equal(1428, res)
    
[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "2,0,6,12,1,3"
    Assert.Equal(3718541, res)

[<Theory>]
[<InlineData(436, "0,3,6")>]
[<InlineData(1, "1,3,2")>]
[<InlineData(10, "2,1,3")>]
[<InlineData(27, "1,2,3")>]
[<InlineData(78, "2,3,1")>]
[<InlineData(438, "3,2,1")>]
[<InlineData(1836, "3,1,2")>]
let ``Solve 1 - example 1`` expected input =
    let res = solve1 input

    Assert.Equal(expected, res)
