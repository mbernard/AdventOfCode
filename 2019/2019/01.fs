module _01

open Common
open Xunit

let getFuel x = x / 3 - 2 

let rec getFuel2 x = 
    getFuel x
    |> function
        | y when y > 0 -> 
            getFuel2 y
            |> (+) y
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

[<Fact>]
let ``Test solve2`` () =
    solve2

[<Theory>]
[<InlineData(14, 2)>]
[<InlineData(1969, 966)>]
[<InlineData(100756, 50346)>]
let ``getFuel2 test`` input expected =
    let actual = getFuel2 input
    Assert.Equal(expected, actual)
    