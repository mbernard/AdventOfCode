module day12

open System
open Xunit
open Common
open FsUnit
open System.Text.RegularExpressions
open System.Text.Json;

[<Theory>]
[<InlineData(3, "../../../Solutions/12/data_test.txt")>]
[<InlineData(111754, "../../../Solutions/12/data.txt")>]
let solve1 expected path =
    path
    |> parseFirstLine extractInts
    |> Array.sum
    |> should equal expected

let replace (s: char array) =
    let mutable levels = []
    let mutable i = 0
    while i < s.Length do
        match s[i] with
        | '{' -> 
            levels <- (i,false)::levels
            ()
        | '}' -> 
            let (startI,redInScope)::rest = levels
            levels <- rest
            if redInScope then
                // redact
                for j in [startI..i] do
                    s[j] <- ' '
            ()
        | 'd' when s[i-1] = 'e' && s[i-2] = 'r' && s[i-3] = '"' && s[i-4] = ':' ->
            let (startI,redInScope)::rest = levels
            levels <- (startI,true)::rest
            ()
        | _ -> ()
        i <- i + 1
    s
    

[<Theory>]
[<InlineData(0, "../../../Solutions/12/data_test3.txt")>]
[<InlineData(4, "../../../Solutions/12/data_test2.txt")>]
[<InlineData(65402, "../../../Solutions/12/data.txt")>]
let solve2 expected path =
    path
    |> parseFirstLine asCharArray
    |> replace
    |> charsToStr
    |> extractInts
    |> Array.sum
    |> should equal expected