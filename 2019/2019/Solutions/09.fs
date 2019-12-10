module _09

open FSharpx.Collections
open Common
open Computer

let solve input program =
    Computer.initialize program (Queue.ofList [ input ])
    |> Computer.executeUntilHalt
    |> Computer.tryReadFromOutput
    |> snd
    |> Option.defaultValue 0L

open Xunit

[<Fact>]
let ``Test solve 2`` () =
    let actual =
        "../../../Data/09.txt"
        |> parseFirstLine (splitBy "," asIntArray)
        |> solve 2L
    Assert.Equal(46643L, actual)

[<Fact>]
let ``Test solve 1`` () =
    let actual =
        "../../../Data/09.txt"
        |> parseFirstLine (splitBy "," asIntArray)
        |> solve 1L
    Assert.Equal(2955820355L, actual)

[<Fact>]
let ``getvalue test`` () =
    let program = [0..50] |> Array.ofList |> Array.setAt 0 -7
    let c = Computer.initialize program (Queue.ofList [])
    let actual =
        { c with RelativeBase = 50L }
        |> Computer.getValue (Relative -7L)
    Assert.Equal(43L, actual)

[<Fact>]
let ``ajust relative base test`` () =
    let program = [|109;19;99|]
    let c = Computer.initialize program (Queue.ofList [])
    let actual =
        { c with RelativeBase = 2000L }
        |> Computer.executeUntilHalt
    Assert.Equal(2019L, actual.RelativeBase)

[<Fact>]
let ``ajust relative base test2`` () =
    let program = [|109;19;204;-34;99|] 
    let c = Computer.initialize program Queue.empty
    let actual =
        { c with RelativeBase = 2000L; Memory = Map.add 1985L 9L c.Memory }
        |> Computer.executeUntilHalt
        |> Computer.tryReadFromOutput
        |> snd
        |> Option.defaultValue 0L

    Assert.Equal(9L, actual)

[<Fact>]
let ``copy itself`` () =
    let program = [|109;1;204;-1;1001;100;1;100;1008;100;16;101;1006;101;0;99|]
    let c = Computer.initialize program (Queue.ofList [])
    let actual = c |> Computer.executeUntilHalt

    Assert.Equal<int64>(program |> Array.toSeq |> Seq.map int64, actual.Output |> Queue.toSeq)

[<Fact>]
let ``16 digit number`` () =
    let program = [|1102;34915192;34915192;7;4;7;99;0|]
    let c = Computer.initialize program (Queue.ofList [])
    let actual = 
        c 
        |> Computer.executeUntilHalt
        |> Computer.tryReadFromOutput
        |> snd
        |> Option.defaultValue 0L

    Assert.Equal(16, actual |> string |> String.length)

[<Fact>]
let ``large number`` () =
    let program = [|104L;1125899906842624L;99L|]
    let c = Computer.initialize64 program (Queue.ofList [])
    let actual = 
        c 
        |> Computer.executeUntilHalt
        |> Computer.tryReadFromOutput
        |> snd
        |> Option.defaultValue 0L

    Assert.Equal(1125899906842624L, actual)