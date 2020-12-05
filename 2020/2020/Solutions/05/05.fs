module _05

open System.Globalization
open Xunit
open Common
open System

let toBinary (x:string) =
    x.Replace('F', '0')
        .Replace('B','1')
        .Replace('L','0')
        .Replace('R','1')
    
let split (x:string) =
    let row = Convert.ToInt32(x.Substring(0, 7), 2)
    let column = Convert.ToInt32(x.Substring(7, 3), 2)
    (row, column)
    
let seatId (r,c) = r * 8 + c

let solve1 data =
    data
    |> parseEachLine asString
    |> Seq.map toBinary
    |> Seq.map split
    |> Seq.map seatId
    |> Seq.max

[<Theory>]
[<InlineData("FBFBBFFRLR")>]
let ``Solve 1 - Test 1`` s =
    let res = toBinary s |> split |> seatId
    Assert.Equal(357, res)

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/05/data.txt"
    Assert.Equal(0, res)
