module _02

open Xunit
open Common

let isValid (args: string[]) =
    let min = args.[0] |> int
    let max = args.[1] |> int
    let c = args.[2] |> char
    let pass =
        args.[3].ToCharArray()
        |> Array.filter (fun x -> x = c)
        |> Array.length
    min <= pass && pass <= max

let solve1 data =
    data
    |> parseEachLine (withRegex "(\d+)-(\d+) ([a-z]): (.+)")
    |> Seq.filter isValid
    |> Seq.length
    

[<Fact>]
let ``Solve 1``() =
    let res = solve1 "../../../Solutions/02/data.txt"
    Assert.Equal(0, res)
    
[<Fact>]
let ``Solve 1 - Tests 1``() =
    let res = solve1 "../../../Solutions/02/data-test-1.txt"
    Assert.Equal(2, res)
