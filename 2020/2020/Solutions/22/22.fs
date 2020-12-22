module _22

open System
open System.Text.RegularExpressions
open Xunit
open Common


let parseDeck (x :: xs) =
    xs
    |> List.filter (fun x -> x <> String.Empty)
    |> List.map int

let rec play (decks: int list list) =
    let x :: xs = List.item 0 decks
    let y :: ys = List.item 1 decks

    let newDecks =
        if x > y then [ xs @ [ x; y ]; ys ] else [ xs; ys @ [ y; x ] ]

    if xs = List.Empty || ys = List.Empty then newDecks else play newDecks

let score deck =
    List.foldBack (fun x (score, i) -> (score + i * x, i + 1)) deck (0, 1)
    |> fst

let parseDecks lines =
    lines
    |> List.splitInto 2
    |> List.map parseDeck
    |> play
    |> List.filter (fun d -> d <> List.Empty)
    |> List.exactlyOne
    |> score

let solve1 data =
    data
    |> parseEachLine asString
    |> Seq.toList
    |> parseDecks

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/22/data.txt"
    Assert.Equal(34127, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/22/data-test-1.txt"

    Assert.Equal(306, res)
