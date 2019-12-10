module Computer

open FSharpx.Collections

type Mode =
    | Position
    | Immediate
    | Relative

type Instruction =
    { OpCode: int
      Modes: Mode [] }

type State =
    | Executing
    | Done

type Computer =
    { ExecutingIndex: int64
      Memory: Map<int64,int64>
      State: State
      Input: Queue<int64>
      Output: Queue<int64>
      LastSignal: int64
      RelativeBase: int64 }

let initialize program input =
    { ExecutingIndex = 0L
      Memory = program |> Array.mapi (fun i x -> (int64 i, int64 x)) |> Map.ofArray 
      State = Executing
      Input = input
      Output = Queue.empty
      LastSignal = 0L
      RelativeBase = 0L}

let parseMode =
    function
    | 0 -> Position
    | 1 -> Immediate
    | 2 -> Relative
    | x -> failwithf "%i is an invalid mode" x

let getValue (mode, indexValue:int64) computer =
    match mode with
    | Position -> computer.Memory |> Map.tryFind indexValue |> Option.defaultValue 0L
    | Immediate -> indexValue
    | Relative -> computer.Memory.[computer.RelativeBase + indexValue]

let setValue key value computer =
    { computer with Memory = computer.Memory.Add(key, value) }

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

let getParam1 (modes:Mode []) c =
        getValue (modes.[0], c.Memory.[c.ExecutingIndex + 1L]) c

let getParam2 (modes:Mode []) c =
    [ 
        getValue (modes.[0], c.Memory.[c.ExecutingIndex + 1L]) c
        getValue (modes.[1], c.Memory.[c.ExecutingIndex + 2L]) c
    ]

let getParam3 (modes:Mode []) c =
    [ 
        getValue (modes.[0], c.Memory.[c.ExecutingIndex + 1L]) c
        getValue (modes.[1], c.Memory.[c.ExecutingIndex + 2L]) c
        getValue (modes.[2], c.Memory.[c.ExecutingIndex + 3L]) c
    ]

let advance x c =
    { c with ExecutingIndex = c.ExecutingIndex + x }

let private readNextInstruction c =
    getValue (Position,c.ExecutingIndex) c
    |> parseInstruction
    |> (fun instruction ->
    match instruction.OpCode with
    | 1 ->
        instruction.Modes.[2] <- Immediate
        let p = getParam3 instruction.Modes c
        setValue p.[2] (p.[0] + p.[1]) c
        |> advance 4L
    | 2 ->
        instruction.Modes.[2] <- Immediate
        let p = getParam3 instruction.Modes c
        setValue p.[2] (p.[0] * p.[1]) c
        |> advance 4L
    | 3 ->
        let (v, rest) = c.Input |> Queue.uncons
        let p = getParam1 [|Immediate|] c
        setValue p v c
        |> (fun x -> { x with Input = rest })
        |> advance 2L
    | 4 ->
        let value = getParam1 instruction.Modes c
        { c with
              Output = (c.Output |> Queue.conj value) 
              LastSignal = value }
        |> advance 2L
    | 5 ->
        let p = getParam2 instruction.Modes c
        if p.[0] <> 0L then { c with ExecutingIndex = p.[1] }
        else c |> advance 3L
    | 6 ->
        let p = getParam2 instruction.Modes c
        if p.[0] = 0L then { c with ExecutingIndex = p.[1] }
        else c |> advance 3L
    | 7 ->
        instruction.Modes.[2] <- Immediate
        let p = getParam3 instruction.Modes c
        let value = if p.[0] < p.[1] then 1L else 0L
        setValue p.[2] value c
        |> advance 4L
    | 8 ->
        instruction.Modes.[2] <- Immediate
        let p = getParam3 instruction.Modes c
        let value = if p.[0] = p.[1] then 1L else 0L
        setValue p.[2] value c
        |> advance 4L
    | 9 -> 
        let value = getParam1 instruction.Modes c
        { c with RelativeBase = c.RelativeBase + value; ExecutingIndex = c.ExecutingIndex + 2L}
    | 99 -> { c with State = Done }
    | x -> failwithf "Unknown OpCode %i" x)

let rec executeUntilOutput c =
    if c.Output
       |> Queue.isEmpty
       |> not
       || c.State = Done
    then c
    else readNextInstruction c |> executeUntilOutput

let rec executeUntilHalt c =
    if c.State = Done
    then c
    else readNextInstruction c |> executeUntilHalt

let tryReadFromOutput c =
    match Queue.tryUncons c.Output with
    | Some(i, is) -> { c with Output = is }, Some i
    | None -> c, None
