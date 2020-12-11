module _11

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
    Assert.Equal(0, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/11/data-test-1.txt"

    Assert.Equal(37, res)
