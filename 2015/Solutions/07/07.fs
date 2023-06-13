module day07

open System
open Xunit
open Common
open FsUnit

type Operation =
    | Not of string
    | Or of string * string
    | And of string * string
    | LShift of string * int
    | RShift of string * int
    | Set of string

type Value =
    | Instruction of Operation
    | Result of uint16

let rec solve (key:string) (map: Map<string,Value>) =
    if UInt16.TryParse key |> fst then uint16 key,map
    else
        match map[key] with
        | Result x -> x, map
        | Instruction op -> 
            let res, resMap =
                match op with
                | Not y ->
                    let v,m = solve y map
                    ~~~v,m
                | Or (y, z) -> 
                    let v,m = solve y map
                    let v2,m2 = solve z m
                    v ||| v2, m2
                | And (y, z) ->
                    let v,m = solve y map
                    let v2,m2 = solve z m
                    v &&& v2, m2
                | LShift (y, z) ->
                    let v,m = solve y map
                    v<<< z, m
                | RShift (y, z) ->
                    let v,m = solve y map
                    v>>> z, m
                | Set y -> solve y map
            res, Map.add key (Result res) resMap
                

let buildGraph (map: Map<string, Value>) (xs: string array) =
    let value =
        match xs[0] with
        | Regex "^(\d+)$" [ x ] -> Result(uint16 x)
        | Regex "NOT (.*)" [ x ] -> Not(x) |> Instruction
        | Regex "(.*) OR (.*)" [ x; y ] -> Or(x, y) |> Instruction
        | Regex "(.*) AND (.*)" [ x; y ] -> And(x, y) |> Instruction
        | Regex "(.*) LSHIFT (\d+)" [ x; i ] -> LShift(x, int i) |> Instruction
        | Regex "(.*) RSHIFT (\d+)" [ x; i ] -> RShift(x, int i) |> Instruction
        | Regex "(.*)" [x] -> Set x |> Instruction
        | x -> failwithf "Unknow operator '%s'" x
    
    Map.add xs[1] value map

[<Fact>]
let solve1 () =
    "../../../Solutions/07/data.txt"
    |> parseEachLine (withRegex "(.*) -> (.*)")
    |> Seq.fold buildGraph Map.empty
    |> solve "a"
    |> fst
    |> should equal 3176

[<Fact>]
let solve2 () =
    "../../../Solutions/07/data2.txt"
    |> parseEachLine (withRegex "(.*) -> (.*)")
    |> Seq.fold buildGraph Map.empty
    |> solve "a"
    |> fst
    |> should equal 14710