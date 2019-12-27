module _15

open Computer
open Common
open Xunit

type Direction =
    | North
    | South
    | West
    | East

type Square = | Wall | Empty | OxygenSystem
type Robot = { Position: int * int; Computer: Computer; }

let initializeRobot program = {Position = 0,0; Computer = Computer.initialize64 program; }

//let turnLeft = 
//    function
//    | North -> West
//    | West -> South
//    | South -> East
//    | East -> North

//let turnRight = 
//    function
//    | North -> East
//    | East -> South
//    | South -> West
//    | West -> North

//let getCoord (x,y) =
//    function
//    | North -> (x, y+1)
//    | South -> (x, y-1)
//    | West -> (x-1, y)
//    | East -> (x+1, y)

//let toInt64 =
//    function
//    | North -> 1L
//    | South -> 2L
//    | West -> 3L
//    | East -> 4L

//let execute r =
//    r.Computer 
//    |> Computer.writeInput (r.Direction |> toInt64)
//    |> Computer.executeUntilOutput
//    |> Computer.readOutput

//let generateMap r =
//    let generateMap' r x =
//        let coord = getCoord r.Position r.Direction
//        let status,c = execute r
//        match status with
//        | 0L -> // Wall
//            {r with 
//                Computer = c; 
//                Map = r.Map |> Map.add coord Wall 
//                Direction = turnLeft r.Direction }
//        | 1L -> // Empty
//            {r with 
//                Computer = c; 
//                Map = r.Map |> Map.add coord Empty 
//                Position = coord
//                Direction = turnRight r.Direction }
//        | 2L -> // Oxygen
//            {r with 
//                Computer = c; 
//                Map = r.Map |> Map.add coord OxygenSystem 
//                Position = coord
//                Direction = turnRight r.Direction }
//        | _ -> failwith "Invalid object"

//    [0..10_000]
//    |> List.fold generateMap' r
    
let getPos direction r =
    let x,y = r.Position
    let newPos = 
        match direction with
        | 1L -> (x, y+1) // North
        | 2L -> (x, y-1) // South
        | 3L -> (x-1, y) // West
        | 4L -> (x+1, y) // East
        | _ -> failwith "Invalid direction"
    { r with Position = newPos }

let run r s direction =
    let o,newC =
        Computer.writeInput direction r.Computer
        |> Computer.executeUntilOutput
        |> Computer.readOutput
    let newR = { r with Computer = newC }
    match o with
    | 0L -> s // Wall
    | 1L // Empty
    | 2L -> // Oxygen
        newR
        |> getPos direction
        |> Array.singleton
        |> Array.append s
    | _ -> failwith "Invalid output"

let getNeiboors s r = 
    [|1L..4L|]
    |> Array.fold (run r) [||]
    |> Array.append s 

let notSeen seen x =
    Set.contains x.Position seen
    |> not

let addToSeen s x =
    Set.add x s

let bfs stopCondition robot = 
    let rec bfs' nodes seen steps  =
        match Array.tryFind stopCondition nodes with
        | Some r -> Some r,steps
        | None ->
            let n =
                nodes
                |> Array.fold getNeiboors [||]
                |> Array.filter (notSeen seen)
            if Array.isEmpty n 
            then None,steps
            else
                let newSeen =
                    n
                    |> Array.map (fun x -> x.Position)
                    |> Array.fold addToSeen seen
                bfs' n newSeen (steps + 1)
    bfs' [|robot|] Set.empty 0

[<Fact>]
let ``solve 2`` () =
    let actual =
        "../../../Data/15.txt"
        |> parseFirstLine (splitBy "," asInt64Array)
        |> initializeRobot
        |> bfs (fun x -> x.Computer.LastSignal = 2L)
        |> fst
        |> Option.get
        |> bfs (fun _ -> false)
        |> snd
    
    Assert.Equal(342, actual)

[<Fact>]
let ``solve 1`` () =
    let actual =
        "../../../Data/15.txt"
        |> parseFirstLine (splitBy "," asInt64Array)
        |> initializeRobot
        |> bfs (fun x -> x.Computer.LastSignal = 2L)
        |> snd

    Assert.Equal(226, actual)