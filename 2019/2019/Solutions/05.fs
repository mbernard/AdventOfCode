module _05

open Common

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
    
let rec readNextInstruction i input (xs: int []) =
    getValue (Position,i) xs
    |> parseInstruction
    |> (fun instruction -> 
        match instruction.OpCode with
        | 1 ->
            xs.[i+1..i+3]
            |> Array.zip instruction.Modes
            |> (fun p -> xs.[snd p.[2]] <- getValue p.[0] xs + getValue p.[1] xs)
            |> ignore

            readNextInstruction (i + 4) input xs
        | 2 -> 
            xs.[i+1..i+3]
            |> Array.zip instruction.Modes
            |> (fun p -> xs.[snd p.[2]] <- getValue p.[0] xs *  getValue p.[1] xs)
            |> ignore

            readNextInstruction (i + 4) input xs
        | 3 -> 
            xs.[xs.[i+1]] <- input
            readNextInstruction (i + 2) input xs
        | 4-> 
            let mode = instruction.Modes |> Array.head
            let value = getValue (mode, xs.[i+1]) xs
            readNextInstruction (i + 2) value xs
        //| 5 ->
        //    if xs.[i+1] <> 0 
        //    then readNextInstruction xs.[i+2] input xs
        //    else readNextInstruction (i + 3) input xs
        //| 6 ->
        //    if xs.[i+1] = 0 
        //    then readNextInstruction xs.[i+2] input xs
        //    else readNextInstruction (i + 3) input xs
        //| 7 -> 
        //    xs.[i+3] <- if xs.[i+1] < xs.[i+2] then 1 else 0
        //    readNextInstruction (i+4) input xs
        //| 8 -> 
        //    xs.[i+3] <- if xs.[i+1] = xs.[i+2] then 1 else 0
        //    readNextInstruction (i+4) input xs
        | 99 -> (input, xs)
        | x -> failwithf "Unknown OpCode %i" x)

let solve =
    parse
    |> readNextInstruction 0 1

open Xunit

let values: obj array seq =
    seq {
        yield [| [| 2; 0; 0; 0; 99 |]
                 0
                 [| 1; 0; 0; 0; 99 |] 
                 0 |]
        yield [| [| 2; 3; 0; 6; 99 |]
                 0
                 [| 2; 3; 0; 3; 99 |] 
                 0 |]
        yield [| [| 2; 4; 4; 5; 99; 9801 |]
                 0
                 [| 2; 4; 4; 5; 99; 0 |] 
                 0 |]
        yield [| [| 30; 1; 1; 4; 2; 5; 6; 0; 99 |]
                 0
                 [| 1; 1; 1; 4; 99; 5; 6; 0; 99 |] 
                 0 |]
        yield [| [|1002;4;3;4;99|]
                 0
                 [|1002;4;3;4;33|] 
                 0 |]
        yield [| [|1101;100;-1;4;99|]
                 0
                 [|1101;100;-1;4;0|]
                 0 |]
        yield [| [|123;0;4;0;99|]
                 123
                 [|3;0;4;0;99|]
                 123 |]
    }

[<Theory>]
[<MemberData("values")>]
let ``read next instruction`` (expectedArray, expectedOutput, inputArray, input) =
    let (output, array) = readNextInstruction 0 input inputArray
    Assert.Equal<int>(expectedArray, array)
    Assert.Equal(expectedOutput, output)

[<Fact>]
let ``solve 1`` () =
    let actual = fst solve
    Assert.Equal(5044655, actual) 
  
 