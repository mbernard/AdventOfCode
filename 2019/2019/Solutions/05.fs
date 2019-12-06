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
    
let getValue mode (x:int) (xs: int []) =
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

    { OpCode =  opCode ; Modes = modes |> Array.map (int >> parseMode) }
    

let apply (xs: int []) read1 read2 write f =
    xs.[write] <- (f xs.[read1] xs.[read2])
    xs

let getFunc i xs =
    let sub = Array.sub xs i 4
    apply xs sub.[1] sub.[2] sub.[3]

let rec readNextInstruction i input (xs: int []) =
    getValue Position i xs
    |> parseInstruction
    |> function
       | { OpCode = 1 } ->
        
            getFunc i xs (+) |> readNextInstruction (i + 4)
       | { OpCode = 2 } -> getFunc i xs (*) |> readNextInstruction (i + 4)
       | { OpCode = 3} -> getFunc i xs (*) |> readNextInstruction (i + 1) 
       |  {OpCode = 4 }-> getFunc i xs (*) |> readNextInstruction (i + 1) 
       | {OpCode = 99} -> xs
       | _ -> failwith "Unknown OpCode"

let solve x =
    parse
    |> readNextInstruction 0 1
  
 