module Computer

open FSharpx.Collections

type Param = 
    | Position of int64
    | Immediate of int64
    | Relative of int64

type Instruction =
    { OpCode: int
      Modes: (int64 -> Param) [] }

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

let initialize64 program =
    { ExecutingIndex = 0L
      Memory = program |> Array.mapi (fun i x -> (int64 i, x)) |> Map.ofArray 
      State = Executing
      Input = Queue.empty
      Output = Queue.empty
      LastSignal = 0L
      RelativeBase = 0L}

let initialize program =
    initialize64 (program |> Array.map int64)

let writeInput x c = { c with Input = Queue.conj x c.Input}
let writeInputArray x c = 
    x |> 
    Seq.fold (fun s y -> { s with Input = Queue.conj y s.Input}) c

let parseMode =
    function
    | 0 -> Position
    | 1 -> Immediate
    | 2 -> Relative
    | x -> failwithf "%i is an invalid mode" x

let rec getValue param computer =
    match param with
    | Position x -> computer.Memory |> Map.tryFind x |> Option.defaultValue 0L
    | Immediate x -> x
    | Relative x -> getValue (Position (computer.RelativeBase + x)) computer

let rec setValue param value computer =
    match param with
    | Position x -> { computer with Memory = computer.Memory.Add(x, value) }
    | Immediate x -> failwith "setValue invalid in immediate mode"
    | Relative x -> setValue (Position (x + computer.RelativeBase)) value computer

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
          |> Array.rev
          |> Array.map
              (string
               >> int
               >> parseMode)
          }

let getParam1 (modes: (int64 -> Param)[]) c =
    getValue (Position (c.ExecutingIndex + 1L)) c |> modes.[0]
    
let getParam2 (modes: (int64 -> Param)[]) c =
    [ 
        getValue (Position (c.ExecutingIndex + 1L)) c |> modes.[0]
        getValue (Position (c.ExecutingIndex + 2L)) c |> modes.[1]
    ]

let getParam3 (modes: (int64 -> Param)[]) c =
    [ 
        getValue (Position (c.ExecutingIndex + 1L)) c |> modes.[0]
        getValue (Position (c.ExecutingIndex + 2L)) c |> modes.[1]
        getValue (Position (c.ExecutingIndex + 3L)) c |> modes.[2]
    ]

let advance x c =
    { c with ExecutingIndex = c.ExecutingIndex + x }

let private readNextInstruction c =
    getValue (Position c.ExecutingIndex) c
    |> parseInstruction
    |> (fun instruction ->
    match instruction.OpCode with
    | 1 ->
        let p = getParam3 instruction.Modes c
        setValue p.[2] (getValue p.[0] c + getValue p.[1] c) c
        |> advance 4L
    | 2 ->
        let p = getParam3 instruction.Modes c
        setValue p.[2] (getValue p.[0] c * getValue p.[1] c) c
        |> advance 4L
    | 3 ->
        let (v, rest) = c.Input |> Queue.uncons
        let p = getParam1 instruction.Modes c
        setValue p v c
        |> (fun x -> { x with Input = rest })
        |> advance 2L
    | 4 ->
        let p = getParam1 instruction.Modes c
        let value = getValue p c
        { c with
              Output = (c.Output |> Queue.conj value) 
              LastSignal = value }
        |> advance 2L
    | 5 ->
        let p = getParam2 instruction.Modes c
        if getValue p.[0] c <> 0L then { c with ExecutingIndex = getValue p.[1] c }
        else c |> advance 3L
    | 6 ->
        let p = getParam2 instruction.Modes c
        if getValue p.[0] c = 0L then { c with ExecutingIndex = getValue p.[1] c }
        else c |> advance 3L
    | 7 ->
        let p = getParam3 instruction.Modes c
        let value = if getValue p.[0] c < getValue p.[1] c then 1L else 0L
        setValue p.[2] value c
        |> advance 4L
    | 8 ->
        let p = getParam3 instruction.Modes c
        let value = if getValue p.[0] c = getValue p.[1] c then 1L else 0L
        setValue p.[2] value c
        |> advance 4L
    | 9 -> 
        let p = getParam1 instruction.Modes c
        let value = getValue p c
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

let rec executeUntilInput c =
    if c.State = Done || getValue (Position c.ExecutingIndex) c = 3L && Queue.isEmpty c.Input
    then c
    else readNextInstruction c |> executeUntilInput

let tryReadFromOutput c =
    match Queue.tryUncons c.Output with
    | Some(i, is) -> { c with Output = is },Some i
    | None -> c,None

let readOutput c =
    let i, is = Queue.uncons c.Output
    i, { c with Output = is }