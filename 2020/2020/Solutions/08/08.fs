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
    

let solve1 data =
    data
    |> parseEachLine (withRegex @"^(.*) ([+|-]\d+)$")
    |> Seq.map toInstruction
    |> Seq.toArray
    |> execute 0 0 []

[<Fact>]
let ``Solve 1``()=
    let res = solve1 "../../../Solutions/08/data.txt"
    Assert.Equal(1521, res)
    
[<Fact>]
let ``Solve 1 - example 1``()=
    let res = solve1 "../../../Solutions/08/data-test-1.txt"
    Assert.Equal(5, res)

