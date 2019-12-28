module _16

open Common
open Xunit
open System.Linq

let generatePattern pattern length =
    Enumerable.Repeat
    let pLength = pattern |> Array.length
    seq {
        for i in [0 .. length - 1] do
            yield Array.item (i % pLength) pattern
    }

let runPhase pattern s phaseNumber =
    Array.zip pattern s
    |> Array.map (fun (x,y) -> x * y)
    |> Array.sum

    s 

let run phase input =
    let basePattern = generatePattern [|0;1;0;-1|] (input |> Array.length)
    [1..phase]
    |> List.fold (runPhase basePattern) input
    ()


[<Theory>]
[<InlineData("01029498", 4, "12345678")>]
let ``solve 1`` expected phases input =
    let actual = ""
    Assert.Equal(expected, actual)
