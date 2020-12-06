module _06

open Xunit
open Common

let assemble (xs: string list, current: string option) (x: string) =
    if x = "" then
        let c = current |> Option.get
        (c :: xs, None)
    else
        let c =
            match current with
            | None -> x
            | Some s -> $"{s}{x}"
            |> Some

        (xs, c)
        
let addLastItem (xs,x) = (x |> Option.defaultValue "")::xs

let solve1 data =
    data
    |> parseEachLine asString
    |> Seq.fold assemble ([], None)
    |> addLastItem
    |> List.map asCharArray
    |> List.map (Array.distinct >> Array.length)
    |> List.sum

[<Fact>]
let ``Solve 1``() =
    let res = solve1 "../../../Solutions/06/data.txt"
    Assert.Equal(6521, res)
    
[<Fact>]
let ``Solve 1 - Example 1``() =
    let res = solve1 "../../../Solutions/06/data-test-1.txt"
    Assert.Equal(11, res)