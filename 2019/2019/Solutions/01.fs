module _01

open Common

let getFuel x = x / 3 - 2

let rec getFuel2 x =
    getFuel x
    |> function
    | y when y > 0 -> getFuel2 y |> (+) y
    | _ -> 0

let solve1 =
    "../../../Data/01.txt"
    |> parseEachLine asInt
    |> Seq.map getFuel
    |> Seq.reduce (+)

let solve2 =
    "../../../Data/01.txt"
    |> parseEachLine asInt
    |> Seq.map getFuel2
    |> Seq.reduce (+)
    |> printf "%i"
 