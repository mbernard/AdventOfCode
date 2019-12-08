module Computer

open FSharpx.Collections

type Mode =
    | Position
    | Immediate

type Instruction =
    { OpCode: int
      Modes: Mode [] }

type State =
    | Executing
    | Done

type Computer =
    { ExecutingIndex: int
      Memory: int array
      State: State
      Input: Queue<int>
      Output: Queue<int>
      LastSignal: int }

let initialize program input =
    { ExecutingIndex = 0
      Memory = program
      State = Executing
      Input = input
      Output = Queue.empty
      LastSignal = 0 }

let parseMode =
    function
    | 0 -> Position
    | 1 -> Immediate
    | x -> failwithf "%i is an invalid mode" x

let getValue (mode, (x: int)) (xs: int []) =
    match mode with
    | Position -> xs.[x]
    | Immediate -> x

let parseInstruction x =
    let (modes, opCodeDigits) = (sprintf "%05i" x).ToCharArray() |> Array.splitAt 3

    let opCode =
        opCodeDigits
        |> Array.map string
        |> String.concat ""
        |> int

    { OpCode = opCode
      Modes =
          modes
          |> Array.map
              (string
               >> int
               >> parseMode)
          |> Array.rev }

let private readNextInstruction c =
    getValue (Position, c.ExecutingIndex) c.Memory
    |> parseInstruction
    |> (fun instruction ->
    match instruction.OpCode with
    | 1 ->
        c.Memory.[c.ExecutingIndex + 1..c.ExecutingIndex + 3]
        |> Array.zip instruction.Modes
        |> (fun p -> c.Memory.[snd p.[2]] <- getValue p.[0] c.Memory + getValue p.[1] c.Memory)
        |> ignore

        { c with ExecutingIndex = c.ExecutingIndex + 4 }
    | 2 ->
        c.Memory.[c.ExecutingIndex + 1..c.ExecutingIndex + 3]
        |> Array.zip instruction.Modes
        |> (fun p -> c.Memory.[snd p.[2]] <- getValue p.[0] c.Memory * getValue p.[1] c.Memory)
        |> ignore

        { c with ExecutingIndex = c.ExecutingIndex + 4 }
    | 3 ->
        let (v, rest) = c.Input |> Queue.uncons
        c.Memory.[c.Memory.[c.ExecutingIndex + 1]] <- v

        { c with
              ExecutingIndex = c.ExecutingIndex + 2
              Input = rest }
    | 4 ->
        let mode = instruction.Modes |> Array.head
        let value = getValue (mode, c.Memory.[c.ExecutingIndex + 1]) c.Memory
        { c with
              ExecutingIndex = c.ExecutingIndex + 2
              Output = (c.Output |> Queue.conj value) 
              LastSignal = value }
    | 5 ->
        let param1 = getValue (instruction.Modes.[0], c.Memory.[c.ExecutingIndex + 1]) c.Memory
        let param2 = getValue (instruction.Modes.[1], c.Memory.[c.ExecutingIndex + 2]) c.Memory
        if param1 <> 0 then { c with ExecutingIndex = param2 }
        else { c with ExecutingIndex = c.ExecutingIndex + 3 }
    | 6 ->
        let param1 = getValue (instruction.Modes.[0], c.Memory.[c.ExecutingIndex + 1]) c.Memory
        let param2 = getValue (instruction.Modes.[1], c.Memory.[c.ExecutingIndex + 2]) c.Memory
        if param1 = 0 then { c with ExecutingIndex = param2 }
        else { c with ExecutingIndex = c.ExecutingIndex + 3 }
    | 7 ->
        let values =
            c.Memory.[c.ExecutingIndex + 1..c.ExecutingIndex + 3]
            |> Array.zip instruction.Modes
            |> Array.map (fun (x, y) -> getValue (x, y) c.Memory)

        c.Memory.[c.Memory.[c.ExecutingIndex + 3]] <- if values.[0] < values.[1] then 1
                                                      else 0
        { c with ExecutingIndex = c.ExecutingIndex + 4 }
    | 8 ->
        let values =
            c.Memory.[c.ExecutingIndex + 1..c.ExecutingIndex + 3]
            |> Array.zip instruction.Modes
            |> Array.map (fun (x, y) -> getValue (x, y) c.Memory)

        c.Memory.[c.Memory.[c.ExecutingIndex + 3]] <- if values.[0] = values.[1] then 1
                                                      else 0
        { c with ExecutingIndex = c.ExecutingIndex + 4 }
    | 99 -> { c with State = Done }
    | x -> failwithf "Unknown OpCode %i" x)

let rec execute c =
    if c.Output
       |> Queue.isEmpty
       |> not
       || c.State = Done
    then c
    else readNextInstruction c |> execute

let tryReadFromOutput c =
    match Queue.tryUncons c.Output with
    | Some(i, is) -> { c with Output = is }, Some i
    | None -> c, None
