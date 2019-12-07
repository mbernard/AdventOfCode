module _05

open Common
open FSharpx.Collections

type Mode =
    | Position
    | Immediate

type Instruction = { OpCode : int ; Modes: Mode[]}

let parse = parseFirstLine (splitBy "," asIntArray) "../../../Data/05.txt"
let parseMode  = 
    function
    | 0 -> Position
    | 1 -> Immediate
    | x -> failwithf "%i is an invalid mode" x
    
let getValue (mode,(x:int)) (xs: int []) =
    match mode with
    | Position -> xs.[x]
    | Immediate -> x

let parseInstruction x =
    let (modes, opCodeDigits) = 
        (sprintf "%05i" x).ToCharArray()
        |> Array.splitAt 3
    
    let opCode = 
        opCodeDigits
        |> Array.map string
        |> String.concat ""  
        |> int

    { OpCode =  opCode ; Modes = modes |> Array.map (string >> int >> parseMode) |> Array.rev }
    
let rec readNextInstruction i (input:Queue<int>) (output:Queue<int>) (xs: int []) =
    getValue (Position,i) xs
    |> parseInstruction
    |> (fun instruction -> 
        match instruction.OpCode with
        | 1 ->
            xs.[i+1..i+3]
            |> Array.zip instruction.Modes
            |> (fun p -> xs.[snd p.[2]] <- getValue p.[0] xs + getValue p.[1] xs)
            |> ignore

            readNextInstruction (i + 4) input output xs
        | 2 -> 
            xs.[i+1..i+3]
            |> Array.zip instruction.Modes
            |> (fun p -> xs.[snd p.[2]] <- getValue p.[0] xs *  getValue p.[1] xs)
            |> ignore

            readNextInstruction (i + 4) input output xs
        | 3 -> 
            let (v, rest) = input |> Queue.uncons
            xs.[xs.[i+1]] <- v
            readNextInstruction (i + 2) rest output xs
        | 4-> 
            let mode = instruction.Modes |> Array.head
            let value = getValue (mode, xs.[i+1]) xs
            readNextInstruction (i + 2) input (output |> Queue.conj value) xs
        | 5 ->
            let param1 = getValue (instruction.Modes.[0], xs.[i+1]) xs
            let param2 = getValue (instruction.Modes.[1], xs.[i+2]) xs
            if param1 <> 0
            then readNextInstruction param2 input output xs
            else readNextInstruction (i + 3) input output xs
        | 6 ->
            let param1 = getValue (instruction.Modes.[0], xs.[i+1]) xs
            let param2 = getValue (instruction.Modes.[1], xs.[i+2]) xs
            if param1 = 0
            then readNextInstruction param2 input output xs
            else readNextInstruction (i + 3) input output xs
        | 7 -> 
            let values =
                xs.[i+1..i+3]
                |> Array.zip instruction.Modes
                |> Array.map (fun (x, y) -> getValue (x,y) xs)

            xs.[xs.[i+3]] <- if values.[0] < values.[1] then 1 else 0
            readNextInstruction (i+4) input output xs
        | 8 -> 
            let values =
                xs.[i+1..i+3]
                |> Array.zip instruction.Modes
                |> Array.map (fun (x, y) -> getValue (x,y) xs)

            xs.[xs.[i+3]] <- if values.[0] = values.[1] then 1 else 0
            readNextInstruction (i+4) input output xs
        | 99 -> (output, xs)
        | x -> failwithf "Unknown OpCode %i" x)

//let solve =
//    parse
//    |> readNextInstruction 0 1

let solve2 =
    parse
    |> readNextInstruction 0 (Queue.ofList [5]) Queue.empty

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
    let inputq = Queue.ofList [input]
    let output = readNextInstruction 0 inputq Queue.empty program |> fst |> Queue.toSeq |> Seq.head
    Assert.Equal(expectedOutput, output)

[<Theory>]
[<MemberData("outputArrayTestCases")>]
let ``output array test cases`` (expectedArray, program, input) =
    let inputq = Queue.ofList [input]
    let actualArray = readNextInstruction 0 inputq Queue.empty program |> snd
    Assert.Equal<int>(expectedArray, actualArray)

//[<Fact>]
//let ``solve 1`` () =
//    let actual = fst solve
//    Assert.Equal(5044655, actual) 

[<Fact>]
let ``solve 2`` () =
    let actual = fst solve2 |> Queue.toSeq |> Seq.head
    Assert.Equal(7408802, actual) 
  
 