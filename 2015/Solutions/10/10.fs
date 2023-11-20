module day10

open System
open Xunit
open Common
open FsUnit


let lookAndSay (input: string) =
    let lookAndSayInner (res, currentChar: char, times) x =
        if x = currentChar then
            (res, currentChar, times + 1)
        else
            (res + times.ToString() + currentChar.ToString(), x, 1)

    input.ToCharArray()
    |> (fun xs ->
        xs
        |> Seq.fold lookAndSayInner (String.Empty, xs[0], 0))
    |> (fun (res, currentChar, times) -> res + times.ToString() + currentChar.ToString())

[<Theory>]
[<InlineDataAttribute(6, "1", 5)>]
[<InlineDataAttribute(360154, "1113122113", 40)>]
let solve1 (expected, input: string, times) = 
    {1..times}
    |> Seq.fold (fun s _ -> lookAndSay s)  input
    |> String.length
    |> should equal expected
