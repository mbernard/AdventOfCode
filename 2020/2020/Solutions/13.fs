module _13

open Common
open Computer
open Xunit

let read computer = computer |> tryReadFromOutput
let readAcc (s,computer) x =
    let c,x = read computer
    match x with
    | Some i -> (i::s,c)
    | None -> (s, c)

let readX i computer = [ 1 .. i ] |> List.fold readAcc ([], computer)

let rec updateMap (computer,m,s) =
    let xs,c = computer |> readX 3
    match xs with
    | [] -> c,m,s
    | o::y::x::_ -> 
         if x = -1L && y = 0L then updateMap (c,m,o)
         else updateMap (c,Map.add (x,y) o m,s)
    | _ -> failwith "invalid"

let initializeGame computer = updateMap (computer,Map.empty,0L)

let getPaddleX m = m |> Map.findKey (fun k v -> v = 3L) |> fst
let getBallX m = m |> Map.findKey (fun k v -> v = 4L) |> fst
let tryFindBlock m = m |> Map.tryFindKey (fun k v -> v = 2L)
let rec play (c,m,s) = 
    match tryFindBlock m with
    | None -> s
    | Some b ->
        let move = 
            if getPaddleX m > getBallX m then -1L 
            elif getPaddleX m < getBallX m then 1L
            else 0L
        let computer = 
            writeInput move c
            |> executeUntilInput
        updateMap (computer,m,s)
        |> play

[<Fact>]
let ``Solve 2`` () =
    let actual =
        "../../../Data/13.txt"
        |> parseFirstLine (splitBy "," asInt64Array)
        |> (fun x -> 
            x.[0] <- 2L
            x)
        |> initialize64
        |> executeUntilInput
        |> initializeGame
        |> play

    Assert.Equal(23981L, actual)

[<Fact>]
let ``Solve 1`` () =
    let actual =
        "../../../Data/13.txt"
        |> parseFirstLine (splitBy "," asInt64Array)
        |> initialize64
        |> executeUntilInput
        |> initializeGame
        |> (fun (x,y,z) -> y)
        |> Map.filter (fun k v -> v = 2L)
        |> Map.count
    Assert.Equal(462, actual)