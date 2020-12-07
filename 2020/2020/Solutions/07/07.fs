module _07

open System.Text.RegularExpressions
open Xunit
open Common

type BagRule =
    { Name: string
      Bags: (int * string) [] }

let parseBag s =
    let groups = Regex.Match(s, @"(\d+) (.+)").Groups
    (groups.[1].Value |> int, groups.[2].Value)

let parseLine (line: string array) =
    let bags =
        if line.[1] = "no other bags" then
            [||]
        else
            line.[1]
                .Replace(" bags", "")
                .Replace(" bag", "")
                .Split(", ")
            |> Array.map parseBag

    { Name = Array.head line; Bags = bags }

let rec whoCanHold name (rules: seq<BagRule>) =
    let names =
        rules
        |> Seq.filter (fun y -> y.Bags |> Array.exists (fun (i, n) -> n = name))

    let other =
        names
        |> Seq.map (fun x -> whoCanHold x.Name rules)
        |> Seq.concat

    seq {
        names
        other
    }
    |> Seq.concat
    
let rec countBags name (rules: seq<BagRule>) =
    rules
    |> Seq.find (fun x -> x.Name = name)
    |> (fun x -> x.Bags)
    |> Array.map (fun (i,x) -> i + i * (countBags x rules))
    |> Array.sum

let solve2 data =
    data
    |> parseEachLine (withRegex @"^(.*)? bags contain (.*)\.$")
    |> Seq.map parseLine
    |> countBags "shiny gold"
    
[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/07/data.txt"

    Assert.Equal(39645, res)

[<Fact>]
let ``Solve 2 - example 1`` () =
    let res =
        solve2 "../../../Solutions/07/data-test-1.txt"

    Assert.Equal(32, res)
    
[<Fact>]
let ``Solve 2 - example 2`` () =
    let res =
        solve2 "../../../Solutions/07/data-test-2.txt"

    Assert.Equal(126, res)
    
    
let solve1 data =
    data
    |> parseEachLine (withRegex @"^(.*)? bags contain (.*)\.$")
    |> Seq.map parseLine
    |> whoCanHold "shiny gold"
    |> Seq.map (fun x -> x.Name)
    |> Seq.distinct
    |> Seq.length

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/07/data.txt"

    Assert.Equal(172, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/07/data-test-1.txt"

    Assert.Equal(4, res)
