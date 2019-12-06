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
            let index = getValue (instruction.Modes |> Array.head , xs.[i+1]) xs
            xs.[index] <- input

            readNextInstruction (i + 2) input xs
        | 4-> 
            xs.[i+1..i+1]
            |> Array.zip instruction.Modes
            |> (fun p -> p |> Array.map (fun x -> getValue x xs))
            |> (fun x -> readNextInstruction (i + 2) x.[0] xs)
        | 99 -> (input, xs)
        | x -> failwithf "Unknown OpCode %i" x)

let solve =
    parse
    |> readNextInstruction 0 1

open Xunit

let values: obj array seq =
    seq {
        yield [| [| 2; 0; 0; 0; 99 |]
                 [| 1; 0; 0; 0; 99 |] |]
        yield [| [| 2; 3; 0; 6; 99 |]
                 [| 2; 3; 0; 3; 99 |] |]
        yield [| [| 2; 4; 4; 5; 99; 9801 |]
                 [| 2; 4; 4; 5; 99; 0 |] |]
        yield [| [| 30; 1; 1; 4; 2; 5; 6; 0; 99 |]
                 [| 1; 1; 1; 4; 99; 5; 6; 0; 99 |] |]
    }

[<Theory>]
[<MemberData("values")>]
let ``read next instruction`` (expected, input) =
    let actual = 
        readNextInstruction 0 0 input
        |> snd
    Assert.Equal<int>(expected, actual)

[<Fact>]
let ``solve 1`` () =
    let actual = fst solve
    Assert.Equal(0, actual) 

[<Fact>]
let ``test 1`` () =
    let actual = 
        [|1002;4;3;4;33|]
        |> readNextInstruction 0 0
    Assert.Equal(0, fst actual) 
  
 