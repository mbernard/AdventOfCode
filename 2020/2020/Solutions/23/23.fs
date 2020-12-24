module _23

open System
open System.Text.RegularExpressions
open Xunit
open Common

type Cup = Cup of int

type GameState = { CurrentCup: Cup; Cups: Cup list }

let parse (s: string) =
    s.ToCharArray()
    |> Array.toList
    |> List.map (string >> int)

let rec selectDestinationIndex value rest =
    if value = 0 then
        selectDestinationIndex 9 rest
    else
        rest
        |> List.tryFindIndex (fun x -> x = value)
        |> function
        | None -> selectDestinationIndex (value - 1) rest
        | Some x -> x

let play (x :: xs: int list) round =
    let picks, rest = xs |> List.splitAt 3

    let destIndex = selectDestinationIndex x rest

    let left, y :: right = rest @ [ x ] |> List.splitAt destIndex
    left @ y :: picks @ right

let score xs =
    let i = xs |> List.findIndex (fun x -> x = 1)
    let left, x :: right = xs |> List.splitAt i

    right @ left
    |> List.map string
    |> List.toArray
    |> (fun ys -> String.Join(String.Empty, ys))

let solve1 moves input =
    let game = input |> parse

    let t = [ 1 .. moves ] |> List.fold play game
    t |> score

[<Theory>]
[<InlineData("92658374", 10, "389125467")>]
[<InlineData("67384529", 100, "389125467")>]
[<InlineData("45798623", 100, "398254716")>]
let ``Solve 1`` expected moves input =
    let res = solve1 moves input
    Assert.Equal(expected, res)
