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
    { Position: int*int; Direction: Direction; Map: Map<int*int, char>; Path: int list}
    static member addCharToPath c r = 
        { r with Path = (c|>int)::r.Path }
        

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
        else {r with Path = c::r.Path }
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
    then Robot.addCharToPath d r |> Some
    else None

let tryTurnLeft r =
    {r with Direction = turnLeft r.Direction}
    |> tryTurn 'L'

let tryTurnRight r =
    {r with Direction = turnRight r.Direction}
    |> tryTurn 'R'

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

type Instruction = { A: string; B: string; C: string; Main: string; Path: string}

let rec findPattern s i =
    let x,y = s |> Seq.splitAt i |>  (fun (a,b) -> charsToStr a, charsToStr b)
    if y.Contains(x) 
    then x 
    else findPattern s (i-1)


let buildInstruction ins =
    let initialSplit = ins.Path |> String.length |> (/) 2
    let a = findPattern ins.Path initialSplit
    let restB = ins.Path.Replace(a,"A")
    let b = findPattern restB initialSplit
    let restC = restB.Replace(b,"B")
    ()

[<Fact>]
let ``solve 2`` () =
    let r = 
        generateMap 
        |> toMap 
        |> initializeRobot 
        |> getPathSegment
    { A = ""; B = ""; C = ""; Main = ""; Path = r.Path |> List.rev |> List.map char |> charsToStr }
    |> buildInstruction

    Assert.Equal(0,0)