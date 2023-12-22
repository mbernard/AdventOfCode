module day11

open System
open Xunit
open Common
open FsUnit

let letters =
    [| 'a'
       'b'
       'c'
       'd'
       'e'
       'f'
       'g'
       'h'
       'j'
       'k'
       'm'
       'n'
       'p'
       'q'
       'r'
       's'
       't'
       'u'
       'v'
       'w'
       'x'
       'y'
       'z' |]

let charToIntMap =
    lazy
        (letters
         |> Array.indexed
         |> Array.map (fun (x, y) -> (y, x))
         |> Map.ofArray)

let charToInt x = charToIntMap.Value |> Map.find x

let intToCharMap = lazy (letters |> Array.indexed |> Map.ofArray)

let intToChar x = intToCharMap.Value |> Map.find x

let containsStraight (x: int array) =
    x
    |> Array.indexed
    |> Array.map (fun (i, v) -> v - i)
    |> Array.windowed 3
    |> Array.exists (fun w -> Array.forall (fun v -> v = w[0]) w)

let hasTwoPairs (pass : int array) =
    pass
    |> Array.pairwise
    |> Array.filter (fun (x,y) -> x = y)
    |> Array.distinct
    |> Array.length >= 2

let isValid x =
    containsStraight x 
    && hasTwoPairs x

let rec increment (old: int array) (position: int) =
    old[position] <- (old[position] + 1) % letters.Length

    if old[position] = 0 then
        increment old (position - 1)
    else
        old

let rec nextValidPassword (oldPassword: int array) =
    let newPassword = increment oldPassword (oldPassword.Length - 1)

    if isValid newPassword then
        newPassword
    else
        nextValidPassword newPassword

let findNextPassword (oldPassword: string) =
    oldPassword.ToCharArray()
    |> Array.map charToInt
    |> nextValidPassword
    |> Array.map intToChar
    |> Array.map string
    |> String.concat ""

[<Theory>]
[<InlineData("abcdffaa", "abcdefgh")>]
[<InlineData("vzbxxyzz", "vzbxkghb")>]
[<InlineData("vzcaabcc", "vzbxxyzz")>]
let solve expected (input: string) =
    findNextPassword input |> should equal expected
