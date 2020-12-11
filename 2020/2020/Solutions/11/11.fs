module _11

open FSharpx.Collections
open Xunit
open Common

type Position =
    | Floor
    | EmptySeat
    | OccupiedSeat

let toPosition =
    function
    | '.' -> Floor
    | 'L' -> EmptySeat
    | '#' -> OccupiedSeat
    | _ -> failwith "invalid position"

let toArray2D xs =
    let h = xs |> Array.length
    let w = xs.[0] |> Array.length
    Array2D.init h w (fun x y -> xs.[x].[y] |> toPosition)

let countOccupiedNeighbor r c (xs: Position [,]) =
    xs.[r - 1..r + 1, c - 1..c + 1]
    |> Seq.cast<Position>
    |> Seq.filter (fun x -> x = OccupiedSeat)
    |> Seq.length

let changePosition (xs: Position [,]) r c v =
    let n = countOccupiedNeighbor r c xs

    match v with
    | EmptySeat when n = 0 -> OccupiedSeat
    | OccupiedSeat when n >= 5 -> EmptySeat
    | _ -> v

let rec seatPeople (xs: Position [,]) =
    let ys = Array2D.mapi (changePosition xs) xs

    if ys = xs then xs else seatPeople ys


let solve1 data =
    data
    |> parseEachLine asCharArray
    |> Seq.toArray
    |> toArray2D
    |> seatPeople
    |> Seq.cast<Position>
    |> Seq.filter (fun x -> x = OccupiedSeat)
    |> Seq.length

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/11/data.txt"
    Assert.Equal(2319, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/11/data-test-1.txt"

    Assert.Equal(37, res)

type Direction =
    | N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW

let insideArray x y (xs: Position [,]) =
    x >= 0
    && y >= 0
    && x < Array2D.length1 xs
    && y < Array2D.length2 xs

let rec getFirstSeatInDirection d r c (xs: Position [,]) =
    let (x, y) =
        match d with
        | N -> (r - 1, c)
        | NE -> (r - 1, c + 1)
        | E -> (r, c + 1)
        | SE -> (r + 1, c + 1)
        | S -> (r + 1, c)
        | SW -> (r + 1, c - 1)
        | W -> (r, c - 1)
        | NW -> (r - 1, c - 1)

    
    if insideArray x y xs then 
        match xs.[x, y] with
        | Floor -> getFirstSeatInDirection d x y xs
        | z -> Some z
    else
        None

let getFirstSeatsInSight r c (xs: Position [,]) =
    let t =
          [ getFirstSeatInDirection N r c xs
            getFirstSeatInDirection NE r c xs
            getFirstSeatInDirection E r c xs
            getFirstSeatInDirection SE r c xs
            getFirstSeatInDirection S r c xs
            getFirstSeatInDirection SW r c xs
            getFirstSeatInDirection W r c xs
            getFirstSeatInDirection NW r c xs ]
    t
    |> List.filter (fun x -> x |> Option.isSome)
    |> List.map Option.get
    |> List.filter (fun x -> x = OccupiedSeat)
    |> List.length

let changePosition2 (xs: Position [,]) r c v =
    let n = getFirstSeatsInSight r c xs

    match v with
    | EmptySeat when n = 0 -> OccupiedSeat
    | OccupiedSeat when n >= 5 -> EmptySeat
    | _ -> v

let rec seatPeople2 (xs: Position [,]) =
    let ys = Array2D.mapi (changePosition2 xs) xs

    if ys = xs then xs else seatPeople2 ys

let solve2 data =
    data
    |> parseEachLine asCharArray
    |> Seq.toArray
    |> toArray2D
    |> seatPeople2
    |> Seq.cast<Position>
    |> Seq.filter (fun x -> x = OccupiedSeat)
    |> Seq.length

[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/11/data.txt"
    Assert.Equal(0, res)

[<Fact>]
let ``Solve 2 - example 1`` () =
    let res =
        solve2 "../../../Solutions/11/data-test-1.txt"

    Assert.Equal(26, res)
