module _17

open Common
open System
open Xunit
open FSharpx.Collections
open System.Net

let parse = 
    "../../../Data/17.txt"
    |> parseFirstLine (splitBy "," asInt64Array)
   
let generateMap =
    parse
    |> Computer.initialize64
    |> Computer.executeUntilHalt
    |> (fun x -> x.Output |> Queue.toSeq |> Seq.toArray)
    |> Array.map char
    |> charsToStr
    |> splitBy "\n" (Array.map Seq.toArray) 

let getItem (x,y) map =
    map 
    |> Array.tryItem x
    |> Option.bind (Array.tryItem y) 
    |> Option.defaultValue '.'

let isScaffold x y map = getItem (x,y) map = '#'

let isIntersection x y (map:char[][]) =
    let n =
        [|  getItem ((x-1),y) map
            getItem ((x+1),y) map
            getItem (x,(y-1)) map
            getItem (x,(y+1)) map
        |]
    Array.TrueForAll(n, (fun z -> z = '#'))
    
let getIntersections (map:char[][]) =
    seq {
        for i in [0..map.Length-1] do   
            for j in [0..map.[i].Length-1] do
                if isScaffold i j map && isIntersection i j map 
                then i * j
    }
    |> Seq.sum

[<Fact>]
let ``solve 1`` () =
    let actual = 
        generateMap
        |> getIntersections
    Assert.Equal(8928,actual)

let toMap (map:char[][]) =
    seq {
        for i in [0..map.Length-1] do
            for j in [0..map.[i].Length-1] do
                (i,j),getItem (i,j) map
    }
    |> Map.ofSeq
    
type Direction = 
    | Up
    | Down
    | Right
    | Left

let toDirection = 
    function
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | _ -> invalidOp "toDirection"

type Robot = 
    { Position: int*int; Direction: Direction; Map: Map<int*int, char>; Path: string list}
    static member addIntToPath i r = Robot.addToPath (string i) r
    static member addToPath s r = { r with Path = (s)::r.Path }
        

let initializeRobot m = 
    let pos = m |> Map.findKey (fun k v -> v = '>' || v = '<' || v = 'v' || v = '^')
    let d = m |> Map.find pos |> toDirection
    { Position = pos; Direction = d; Map = m; Path = []}

let getNextPos r = 
    let x,y = r.Position
    match r.Direction with
    | Up -> x-1,y
    | Down -> x+1,y
    | Right -> x,y+1
    | Left -> x,y-1

let tryGetItem x m = m |> Map.tryFind x |> Option.defaultValue '.'

let moveFoward robot =
    let rec moveFowardCount r c = 
        let nextPos = getNextPos r
        if tryGetItem nextPos r.Map = '#' 
        then moveFowardCount {r with Position = nextPos} (c+1)
        else Robot.addIntToPath c r
    moveFowardCount robot 0

let turnRight = 
    function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let turnLeft =
    function 
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

let tryTurn d r = 
    let nextPos = getNextPos r
    if tryGetItem nextPos r.Map = '#'
    then Robot.addToPath d r |> Some
    else None

let tryTurnLeft r =
    {r with Direction = turnLeft r.Direction}
    |> tryTurn "L"

let tryTurnRight r =
    {r with Direction = turnRight r.Direction}
    |> tryTurn "R"

let rec getPathSegment r =
    match tryTurnLeft r with
    | Some newR ->
        newR
        |> moveFoward
        |> getPathSegment
    | None ->
        match tryTurnRight r with
        | Some newR ->
            newR
            |> moveFoward
            |> getPathSegment
        | None -> r

type Instruction = { A: int64[]; B: int64[]; C: int64[]; Main: int64[];}

let containsSubArray xs sub = 
    let subL = Array.length sub
    let xsL = Array.length xs
    let rec containsSubArray' i =
        if i + subL > xsL then false
        else
            let s = Array.sub xs i subL
            if s = sub then true
            else containsSubArray' (i+1)

    containsSubArray' 0

let rec findPattern xs i =
    let x,y = xs |> Array.splitAt i
    if containsSubArray y x
    then x 
    else findPattern xs (i-2)

let replace xs pattern (letter:string) =
    let pL = Array.length pattern
    let xsL = Array.length xs
    let rec replace' (a: string[]) i =
        if i = -1 then a
        elif a.[i.. i + pL-1] = pattern then
            let b = 
                seq {
                    if i = 0 then [||] else a.[..i-1]
                    [|letter|]
                    a.[i + pL..]
                    } |> Array.concat 
                
            replace' b (Array.length b - pL)
        else replace' a (i-1)
    replace' xs (xsL - pL)

let buildPattern x letter max = 
    let y = Array.filter (fun z -> z <> "A" && z <> "B") x
    let pattern = findPattern y max
    let main = replace x pattern  letter
    pattern,main

let toAscii (xs:string []) =
    String.Join(",",xs).ToCharArray()
    |> Array.map int64

let buildInstruction path =
    let a,mainA = buildPattern path "A" 6
    let b,mainB = buildPattern mainA "B" 8
    let c,mainC = buildPattern mainB "C" 8

    { A = toAscii a; B = toAscii b; C = toAscii c; Main = toAscii mainC}

let writeInstructions x c = 
    c
    |> Computer.writeInputArray x.Main
    |> Computer.writeInput (int64 '\n')
    |> Computer.writeInputArray x.A
    |> Computer.writeInput (int64 '\n')
    |> Computer.writeInputArray x.B
    |> Computer.writeInput (int64 '\n')
    |> Computer.writeInputArray x.C
    |> Computer.writeInput (int64 '\n')
    |> Computer.writeInput (int64 'n')
    |> Computer.writeInput (int64 '\n')

let run x =
    let c =
        parse
        |> Array.setAt 0 2L
        |> Computer.initialize64
        |> writeInstructions x
        |> Computer.executeUntilHalt
    c.Output
    |> Queue.toSeq
    |> Seq.rev
    |> Seq.head
    

[<Fact>]
let ``solve 2`` () =
    let r = 
        generateMap 
        |> toMap 
        |> initializeRobot 
        |> getPathSegment
    let res =
        r.Path 
        |> List.rev 
        |> Array.ofList
        |> buildInstruction
        |> run

    Assert.Equal(880360L,res)