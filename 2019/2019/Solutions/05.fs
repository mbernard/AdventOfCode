module _05

open Common
open FSharpx.Collections
open Computer

let parse = parseFirstLine (splitBy "," asIntArray) "../../../Data/05.txt"


let solve =
    (Queue.ofList [1L])
    |> Computer.initialize parse
    |> executeUntilHalt

let solve2 =
    (Queue.ofList [5L])
    |> Computer.initialize parse
    |> executeUntilOutput

open Xunit

let outputArrayTestCases: obj array seq =
    seq {
        yield [| [| 2; 0; 0; 0; 99 |]
                 [| 1; 0; 0; 0; 99 |] 
                 0 |]
        yield [| [| 2; 3; 0; 6; 99 |]
                 [| 2; 3; 0; 3; 99 |] 
                 0 |]
        yield [| [| 2; 4; 4; 5; 99; 9801 |]
                 [| 2; 4; 4; 5; 99; 0 |] 
                 0 |]
        yield [| [| 30; 1; 1; 4; 2; 5; 6; 0; 99 |]
                 [| 1; 1; 1; 4; 99; 5; 6; 0; 99 |] 
                 0 |]
        yield [| [|1002;4;3;4;99|]
                 [|1002;4;3;4;33|] 
                 0 |]
        yield [| [|1101;100;-1;4;99|]
                 [|1101;100;-1;4;0|]
                 0 |]
    }

let outputTestCases: obj array seq =
    seq {
        yield [| 123
                 [|3;0;4;0;99|]
                 123 |]
        // equal to 8 or not
        yield [| 1
                 [|3;9;8;9;10;9;4;9;99;-1;8|]
                 8 |]
        yield [| 0
                 [|3;9;8;9;10;9;4;9;99;-1;8|]
                 5 |]
        // less than 8 or not
        yield [| 1
                 [|3;9;7;9;10;9;4;9;99;-1;8|]
                 4 |]
        yield [| 0
                 [|3;9;7;9;10;9;4;9;99;-1;8|]
                 8 |]
        // equal to 8 or not immediate
        yield [| 1
                 [|3;3;1108;-1;8;3;4;3;99|]
                 8 |]
        yield [| 0
                 [|3;3;1108;-1;8;3;4;3;99|]
                 5 |]
        // less than 8 or not immediate
        yield [| 1
                 [|3;3;1107;-1;8;3;4;3;99|]
                 4 |]
        yield [| 0
                 [|3;3;1107;-1;8;3;4;3;99|]
                 8 |]
        // jmp test if input is zero
        yield [| 0
                 [|3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9|]
                 0 |]
        yield [| 0
                 [|3;3;1105;-1;9;1101;0;0;12;4;12;99;1|]
                 0 |]
        // jmp test if input is non-zero
        yield [| 1
                 [|3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9|]
                 5 |]
        yield [| 1
                 [|3;3;1105;-1;9;1101;0;0;12;4;12;99;1|]
                 5 |]
        // full example
        yield [| 999
                 [|3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99|]
                 5 |]
        yield [| 1000
                 [|3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99|]
                 8 |]
        yield [| 1001
                 [|3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99|]
                 12 |]
    }

[<Theory>]
[<MemberData("outputTestCases")>]
let ``output test cases`` (expectedOutput, program, input) =
    let c = 
        Queue.ofList [int64 input]
        |> Computer.initialize program
        |> executeUntilOutput
    let actual = c.Output |> Queue.head
    Assert.Equal(expectedOutput, actual)

[<Theory>]
[<MemberData("outputArrayTestCases")>]
let ``output array test cases`` (expectedArray, program, input) =
    let c = 
        Queue.ofList [int64 input]
        |> Computer.initialize program
        |> executeUntilOutput

    let actualArray = c.Memory |> Map.toArray |> Array.map snd
    Assert.Equal<int64>(expectedArray |> Array.map int64, actualArray)

[<Fact>]
let ``solve 1`` () =
    let actual = solve.Output |> Queue.ofSeq |> Seq.rev |> Seq.head
    Assert.Equal(5044655L, actual) 

[<Fact>]
let ``solve 2`` () =
    let actual = solve2.Output |> Queue.head
    Assert.Equal(7408802L, actual) 
  
 