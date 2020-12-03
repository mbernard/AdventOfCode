module _03

open Xunit
open Common

let rec countTree total (x,y) (xs:char[][]) (sx,sy) =
    if y >= xs.Length
    then total
    else
        if xs.[y].[x % xs.[0].Length] = '#' then
            countTree (total+1) (x + sx, y + sy) xs (sx, sy)
        else
            countTree total (x + sx, y + sy) xs (sx, sy)
    
    
let solve2 data =
    let slopes = [(1,1); (3,1); (5,1); (7,1); (1,2)]
    
    let map =
        data
        |> parseEachLine asCharArray
        |> Seq.toArray
    
    slopes
    |> List.map (countTree 0 (0,0) map)
    |> List.map int64
    |> List.reduce (*)
    
[<Fact>]
let ``Solve 2``() =
    let res = solve2 "../../../Solutions/03/data.txt"
    Assert.Equal(5007658656L, res)
    
[<Fact>]
let ``Solve 2 - Tests 1``() =
    let res = solve2 "../../../Solutions/03/data-test-1.txt"
    Assert.Equal(336L, res)
    
let solve1 data =
    let map =
        data
        |> parseEachLine asCharArray
        |> Seq.toArray
    countTree 0 (0,0) map (3,1)

[<Fact>]
let ``Solve 1``() =
    let res = solve1 "../../../Solutions/03/data.txt"
    Assert.Equal(164, res)
    
[<Fact>]
let ``Solve 1 - Tests 1``() =
    let res = solve1 "../../../Solutions/03/data-test-1.txt"
    Assert.Equal(7, res)