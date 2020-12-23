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
    lines |> List.splitInto 2 |> List.map parseDeck

let solve1 data =
    data
    |> parseEachLine asString
    |> Seq.toList
    |> parseDecks
    |> play
    |> List.filter (fun d -> d <> List.Empty)
    |> List.exactlyOne
    |> score

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/22/data.txt"
    Assert.Equal(34127, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/22/data-test-1.txt"

    Assert.Equal(306, res)

let rec play2 gameHistory1 gameHistory2 (decks: int list list) =
    let deck1 = List.item 0 decks
    let deck2 = List.item 1 decks

    if  List.contains deck1 gameHistory1
       || List.contains deck2 gameHistory2 then
        [ deck1; [] ]
    else
        let x :: xs = deck1
        let y :: ys = deck2

        let isPlayerOneWinner =
            if x <= List.length xs && y <= List.length ys then
                play2 [] [] [ xs |> List.take x; ys |> List.take y ]
                |> List.item 1
                |> (=) List.Empty
            else
                x > y

        let newDecks =
            if isPlayerOneWinner then [ xs @ [ x; y ]; ys ] else [ xs; ys @ [ y; x ] ]

        if newDecks |> List.contains List.Empty
        then newDecks
        else play2 (deck1 :: gameHistory1) (deck2 :: gameHistory2) newDecks

let solve2 data =
    data
    |> parseEachLine asString
    |> Seq.toList
    |> parseDecks
    |> play2 [] []
    |> List.filter (fun d -> d <> List.Empty)
    |> List.exactlyOne
    |> score

[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/22/data.txt"
    Assert.Equal(31962, res)
// 31962

[<Fact>]
let ``Solve 2 - example 1`` () =
    let res =
        solve2 "../../../Solutions/22/data-test-1.txt"

    Assert.Equal(291, res)
    
[<Fact>]
let ``Solve 2 - infinite loop prevention`` () =
    let res =
        solve2 "../../../Solutions/22/data-test-2.txt"

    Assert.Equal(105, res)
