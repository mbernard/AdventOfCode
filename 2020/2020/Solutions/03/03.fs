module _03

open Xunit
open Common

let rec countTree total x y (xs:char[][]) =
    if y >= xs.Length
    then total
    else
        if xs.[y].[x % xs.[0].Length] = '#' then
            countTree (total+1) (x + 3) (y + 1) xs
        else
            countTree total (x + 3) (y + 1) xs
    

let solve1 data =
    data
    |> parseEachLine asCharArray
    |> Seq.toArray
    |> countTree 0 0 0

[<Fact>]
let ``Solve 1``() =
    let res = solve1 "../../../Solutions/03/data.txt"
    Assert.Equal(164, res)
    
[<Fact>]
let ``Solve 1 - Tests 1``() =
    let res = solve1 "../../../Solutions/03/data-test-1.txt"
    Assert.Equal(7, res)