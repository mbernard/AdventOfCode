module _14

open System
open Microsoft.VisualStudio.TestPlatform.TestHost
open Xunit
open Common
open System.Text.RegularExpressions

type Instruction =
    | Mask of char array
    | Memory of int * int

type Program =
    { Mask: char array
      Memory: Map<int, int64> }

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine line =
    match line with
    | Regex @"mask = (.+)" [ mask ] -> mask.ToCharArray() |> Mask
    | Regex @"mem\[(\d+)\] = (.+)" [ address; value ] -> (address |> int, value |> int) |> Memory
    | _ -> failwith "unable to parse instruction"

let applyMask (mask:char[]) (value: int) =
    let binValue =
        Convert.ToString(value, 2).PadLeft(36,'0').ToCharArray()
        |> Array.mapi (fun i x ->
            match mask.[i] with
            | '1' -> '1'
            | '0' -> '0'
            | 'X' -> x
            | _ -> failwith "invalid mask value")

    Convert.ToInt64(charsToStr binValue, 2)


let execute program x =
    match x with
    | Mask m -> { program with Mask = m }
    | Memory (address, value) ->
        { program with
              Memory =
                  program.Memory
                  |> Map.change address (fun _ -> applyMask program.Mask value |> Some) }

let solve1 data =
    data
    |> parseEachLine parseLine
    |> Seq.fold
        execute
           { Mask = Array.zeroCreate 36
             Memory = Map.empty }
    |> (fun p -> p.Memory)
    |> Map.fold (fun s k v -> s + v) 0L

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/14/data.txt"
    Assert.Equal(15172047086292L, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/14/data-test-1.txt"

    Assert.Equal(165L, res)
