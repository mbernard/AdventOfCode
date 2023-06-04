module day05

open System
open Xunit
open Common
open FsUnit

let isVowel =
    function
    | 'a'
    | 'e'
    | 'i'
    | 'o'
    | 'u' -> true
    | _ -> false

let countVowel = Array.fold (fun s x -> if isVowel x then s + 1 else s) 0

let containSameLetterTwiceInARow xs =
    xs
    |> Array.pairwise
    |> Array.fold (fun s x -> if fst x = snd x then true else s) false

let containsNaugthyString (xs: char array) =
    let x = new string (xs)

    x.Contains("ab")
    || x.Contains("cd")
    || x.Contains("pq")
    || x.Contains("xy")

let isNice xs =
    countVowel xs >= 3
    && containSameLetterTwiceInARow xs
    && not (containsNaugthyString xs)

[<Theory>]
[<InlineData(true, "ugknbfddgicrmopn")>]
[<InlineData(true, "aaa")>]
[<InlineData(false, "aeiou")>]
[<InlineData(false, "xazegov")>]
[<InlineData(false, "aeiouaeiouaeiou")>]
[<InlineData(false, "jchzalrnumimnmhp")>]
[<InlineData(false, "haegwjzuvuyypxyu")>]
[<InlineData(false, "haegwjzuvuyypabu")>]
[<InlineData(false, "haegwjzuvuyypcdu")>]
[<InlineData(false, "haegwjzuvuyyppqu")>]
[<InlineData(false, "dvszwmarrgswjxmb")>]
let testBench (expected: bool) (x: string) =
    x.ToCharArray() |> isNice |> should equal expected

[<Fact>]
let solve1 () =
    "../../../Solutions/05/data.txt"
    |> parseEachLine asCharArray
    |> Seq.fold (fun s x -> if isNice x then s + 1 else s) 0
    |> should equal 238

let hasDoublePair xs =
    let pairs =
        xs
        |> Array.pairwise
        |> Array.mapi (fun i p -> i, p)

    pairs
    |> Array.exists (fun (i, p) ->
        pairs
        |> Array.exists (fun (ii, pp) -> p = pp && ii - i >= 2))

let hasRepeatingLetterWithLetterInBetween xs =
    xs
    |> Array.windowed 3
    |> Array.exists (fun x -> x[0] = x[2])

let isNice2 (xs: char array) =
    hasDoublePair xs
    && hasRepeatingLetterWithLetterInBetween xs

[<Theory>]
[<InlineData(true, "xyxy")>]
[<InlineData(true, "aabcdefgaa")>]
[<InlineData(false, "aaa")>]
[<InlineData(false, "ieodomkazucvgmuy")>]
let test_hasDoublePair (expected: bool) (x: string) =
    x.ToCharArray()
    |> hasDoublePair
    |> should equal expected

[<Theory>]
[<InlineData(true, "xyx")>]
[<InlineData(true, "abcdefeghi")>]
[<InlineData(true, "aaa")>]
let test_hasRepeatingLetterWithLetterInBetween (expected: bool) (x: string) =
    x.ToCharArray()
    |> hasRepeatingLetterWithLetterInBetween
    |> should equal expected

[<Theory>]
[<InlineData(true, "qjhvhtzxzqqjkmpb")>]
[<InlineData(true, "xxyxx")>]
[<InlineData(false, "uurcxstgmygtbstg")>]
[<InlineData(false, "ieodomkazucvgmuy")>]
let test_isNice2 (expected: bool) (x: string) =
    x.ToCharArray()
    |> isNice2
    |> should equal expected

[<Fact>]
let solve2 () =
    "../../../Solutions/05/data.txt"
    |> parseEachLine asCharArray
    |> Seq.fold (fun s x -> if isNice2 x then s + 1 else s) 0
    |> should equal 69
