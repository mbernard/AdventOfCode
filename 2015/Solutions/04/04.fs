module day04

open System
open Xunit
open Common
open FsUnit
open System.Security.Cryptography
open System.Text

let MD5Hash (input : string) =
    use md5 = System.Security.Cryptography.MD5.Create()
    input
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)

let doesMD5hashStartsWithFiveZeros (key:string) (x:int) =
    key + x.ToString()
    |> MD5Hash
    |> (fun x -> x.StartsWith("00000"))

let doesMD5hashStartsWithSixZeros (key:string) (x:int) =
    key + x.ToString()
    |> MD5Hash
    |> (fun x -> x.StartsWith("000000"))

[<Theory>]
[<InlineData(609043, "abcdef")>]
[<InlineData(117946, "ckczppom")>]
let solve1 (expected:int) (key:string) =
    { 0 .. Int32.MaxValue }
    |> Seq.find (doesMD5hashStartsWithFiveZeros key)
    |> should equal expected

[<Theory>]
[<InlineData(3938038, "ckczppom")>]
let solve2 (expected:int) (key:string) =
    { 0 .. Int32.MaxValue }
    |> Seq.find (doesMD5hashStartsWithSixZeros key)
    |> should equal expected


   