module _08

open Xunit
open Common

type Operation = ACC of int | JMP of int | NOP

let toInstruction (xs: string array) =
    let arg = xs.[1] |> int
    match xs.[0] with
    | "acc" -> ACC arg
    | "jmp" -> JMP arg
    | "nop" -> NOP
    | _ -> failwith "invalid instruction"
    
let rec execute acc next executed (xs:Operation[]) :int=
    if List.contains next executed
    then acc
    else
        let e = next::executed
        match xs.[next] with
        | ACC x -> execute (acc + x) (next+1) e xs
        | JMP x -> execute acc (next+x) e xs
        | NOP -> execute acc (next+1) e xs
        
let rec execute2 acc next executed (xs:Operation[])=
    if next >= xs.Length
    then acc |> Ok
    else
        if List.contains next executed
        then Error "infinite loop"
        else
            let e = next::executed
            match xs.[next] with
            | ACC x -> execute2 (acc + x) (next+1) e xs
            | JMP x -> execute2 acc (next+x) e xs
            | NOP -> execute2 acc (next+1) e xs

let rec tryFix i (lines:string [][]) =
    let ogOp = lines.[i].[0]
    let newOp =
        match ogOp with
        | "jmp" -> "nop"
        | "nop" -> "jmp"
        | x -> x
    lines.[i].[0] <- newOp
    lines
    |> Array.map toInstruction
    |> execute2 0 0 []
    |> function
        | Ok x -> x
        | Error x ->
            lines.[i].[0] <- ogOp
            tryFix (i+1) lines
    

let solve2 data =
    data
    |> parseEachLine (withRegex @"^(.*) ([+|-]\d+)$")
    |> Seq.toArray
    |> tryFix 0
 
[<Fact>]
let ``Solve 2``()=
    let res = solve2 "../../../Solutions/08/data.txt"
    Assert.Equal(1016, res)
    
[<Fact>]
let ``Solve 2 - example 1``()=
    let res = solve2 "../../../Solutions/08/data-test-1.txt"
    Assert.Equal(8, res)   

let solve1 data =
    data
    |> parseEachLine (withRegex @"^(.*) ([+|-]\d+)$")
    |> Seq.toArray
    |> Array.map toInstruction
    |> execute 0 0 []

[<Fact>]
let ``Solve 1``()=
    let res = solve1 "../../../Solutions/08/data.txt"
    Assert.Equal(1521, res)
    
[<Fact>]
let ``Solve 1 - example 1``()=
    let res = solve1 "../../../Solutions/08/data-test-1.txt"
    Assert.Equal(5, res)

