module _25

open System
open System.Collections
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharpx.Collections
open Xunit
open Common

let transform value subjectNumber =
    value * subjectNumber % 20201227L

let rec determineLoopSize publicKey value subjectNumber size =
    let v = transform value subjectNumber
    if v = publicKey then size
    else determineLoopSize publicKey v subjectNumber (size + 1)

let solve1 cardPublicKey doorPublicKey =
   let cardLoopSize = determineLoopSize cardPublicKey 1L 7L 1
   let doorLoopSize = determineLoopSize doorPublicKey 1L 7L 1
   let e = [1..cardLoopSize] |> List.fold (fun v _-> transform v doorPublicKey) 1L
   let e2 = [1..doorLoopSize] |> List.fold (fun v _-> transform v cardPublicKey) 1L
   e

[<Theory>]
[<InlineData(14897079L, 5764801L, 17807724L)>]
[<InlineData(4968512L, 10604480L, 4126658)>]
let ``Solve 1`` encryptionKey cardPublicKey doorPublicKey =
    let res = solve1 cardPublicKey doorPublicKey
    Assert.Equal(encryptionKey, res)
