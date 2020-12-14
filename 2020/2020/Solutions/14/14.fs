module _14

open System
open Xunit
open Common
open System.Text.RegularExpressions

type Instruction =
    | Mask of char array
    | Memory of int64 * int64

type Program =
    { Mask: char array
      Memory: Map<int64, int64> }

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine line =
    match line with
    | Regex @"mask = (.+)" [ mask ] -> mask.ToCharArray() |> Mask
    | Regex @"mem\[(\d+)\] = (.+)" [ address; value ] -> (address |> int64, value |> int64) |> Memory
    | _ -> failwith "unable to parse instruction"

let applyMask (mask: char []) (value: int64) =
    let binValue =
        Convert
            .ToString(value, 2)
            .PadLeft(36, '0')
            .ToCharArray()
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

let rec replaceX result (x :: rest) =
    let copy = result |> Array.copy
    let i = copy |> Array.findIndex (fun y -> y = 'X')

    copy.[i] <- x

    match rest with
    | [] -> copy
    | z -> replaceX copy z


let getAddresses (mask: char []) (value: int64) =
    let result =
        Convert
            .ToString(value, 2)
            .PadLeft(36, '0')
            .ToCharArray()
        |> Array.mapi (fun i x ->
            match mask.[i] with
            | '1' -> '1'
            | '0' -> x
            | 'X' -> 'X'
            | _ -> failwith "invalid mask value")

    let xCount =
        result
        |> Array.filter (fun x -> x = 'X')
        |> Array.length
    
 
    if xCount = 0 then [result]
    else
        let possibilities = Math.Pow(2.0, xCount |> float) |> int

        [ 0 .. possibilities - 1 ]
        |> List.map (fun x ->
            Convert.ToString(x, 2).PadLeft(xCount, '0').ToCharArray()
            |> Array.toList)
        |> List.map (replaceX result)
    |> List.map (fun x -> Convert.ToInt64(charsToStr x, 2))

let execute2 program x =
    match x with
    | Mask m -> { program with Mask = m }
    | Memory (address, value) ->
        getAddresses program.Mask address
        |> List.fold (fun s x ->
            { s with
                  Memory = s.Memory |> Map.change x (fun _ -> value |> Some) }) program

let solve2 data =
    data
    |> parseEachLine parseLine
    |> Seq.fold
        execute2
           { Mask = Array.zeroCreate 36
             Memory = Map.empty }
    |> (fun p -> p.Memory)
    |> Map.fold (fun s k v -> s + v) 0L

[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/14/data.txt"
    Assert.Equal(4197941339968L, res)

[<Fact>]
let ``Solve 2 - example 2`` () =
    let res =
        solve2 "../../../Solutions/14/data-test-2.txt"

    Assert.Equal(208L, res)
