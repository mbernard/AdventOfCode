module day06

open System
open Xunit
open Common
open FsUnit

type Direction = 
    | Up
    | Down
    | Left
    | Right

type position = 
    | Guard
    | Empty
    | Obstruction
    | Marked of Direction // when marking a position also remember the direction we came from

let toPosition = 
    function
    | '.' -> Empty
    | '^' -> Guard
    | '#' -> Obstruction
    | _ -> failwith "Invalid character"
    
let parse path = 
    path
    |> parseEachLine asCharArray
    |> array2D
    |> Array2D.map toPosition

let findIndex2D (value: 'T) (array: 'T[,]) : (int * int) option =
    let rows = array.GetLength(0)
    let cols = array.GetLength(1)
    let rec loop r c =
        if r >= rows then None
        elif c >= cols then loop (r + 1) 0
        elif array.[r, c] = value then Some (r, c)
        else loop r (c + 1)
    loop 0 0

let getNextPosition (x, y) d = 
    match d with
    | Up -> (x - 1, y)
    | Down -> (x + 1, y)
    | Left -> (x, y - 1)
    | Right -> (x, y + 1)

let move map =
    let guardI = findIndex2D Guard map |> Option.get
    let rec loop guardI d map moves =
        let x,y = guardI

        // poor's man way to detect infinite loops
        if moves > Array2D.length1 map * Array2D.length2 map then
            None
        else
            // mark current position
            Array2D.set map x y (Marked d)
            match getNextPosition guardI d with
            | nextX, nextY when nextX < 0 || nextY < 0 || nextX >= Array2D.length1 map || nextY >= Array2D.length2 map -> 
                // out of bounds
                map // solution found
                |> Some
            | nextX, nextY when map[nextX, nextY] = Marked d ->
                // we are in a loop
                None
            | nextX, nextY when map.[nextX, nextY] = Obstruction -> 
                // change direction
                loop guardI (match d with | Up -> Right | Right -> Down | Down -> Left | Left -> Up) map (moves+1)
            | nextX, nextY ->
                // move to next position
                loop (nextX, nextY) d map (moves+1)
    
    loop guardI Up map 0

[<Theory>]
[<InlineDataAttribute(41, "../../../Solutions/06/test_data.txt")>]
[<InlineDataAttribute(5086, "../../../Solutions/06/data.txt")>]
let solve1_test expected path =
    path
    |> parse
    |> move
    |> Option.get // there's no infinite loop in problem 1
    |> Seq.cast<position>
    |> Seq.filter (function | Marked _ -> true | _ -> false)
    |> Seq.length
    |> should equal expected
    
let solve2 map =
    let mutable count  = 0
    map
    |> Array2D.iteri (fun x y v -> 
        if v = Empty then
            let copy = Array2D.copy map
            Array2D.set copy x y Obstruction
            match move copy with
            | Some _ -> ()
            | None -> count <- count + 1
        )
    count

[<Theory>]
[<InlineDataAttribute(6, "../../../Solutions/06/test_data.txt")>]
[<InlineDataAttribute(1770, "../../../Solutions/06/data.txt")>]
let solve2_test expected path =
    path
    |> parse
    |> solve2
    |> should equal expected
