module _02

open Common
open Xunit

let parse =
    "../../../Data/02.txt"
    |> parseFirstLine (splitBy "," asIntArray)

let replace (xs:int[]) =
    xs.[1] <- 12
    xs.[2] <- 2
    xs

let apply (xs: int[]) write read1 read2 f =
    xs.[write] <- (f xs.[read1] xs.[read2])
    xs

let getFunc i xs =
    let sub = Array.sub xs i 4
    apply xs sub.[1] sub.[2] sub.[3]

let rec readNextInstruction i (xs: int[]) =
    match xs.[i] with
        | 1 -> 
            getFunc i xs (+)
            |> readNextInstruction (i+4)
        | 2 -> 
            getFunc i xs (*)
            |> readNextInstruction (i+4)
        | 99 -> xs        
        | x -> x |> sprintf "Unknown OpCode %i" |> failwith

let solve1 =
    parse
    |> replace
    |> readNextInstruction 0
    |> Array.item 0


[<Fact>]
let ``read next instruction`` () =
    let input = [|1;0;0;0;99|]
    let expected = [|2;0;0;0;99|]
    let actual = 
        input
        |> readNextInstruction 0
    Assert.Equal<int>(expected, actual)

[<Fact>]
let ``read next instruction 2`` () =
    let input = [|1;0;0;0;99|]
    let expected = [|2;0;0;0;99|]
    let actual = 
        input
        |> readNextInstruction 0
    Assert.Equal<int>(expected, actual)

[<Fact>]
let ``Test solve1`` () =
    Assert.Equal(0, solve1)