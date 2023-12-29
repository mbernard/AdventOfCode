module day13

open System
open Xunit
open Common
open FsUnit

type Instruction =
    { person: string
      neigboor: string
      happiness: int }

let heldKarp (graph: int [,]) =
    let n = graph.GetLength(0)
    let memo = Array2D.create<int> n (1 <<< n) Int32.MinValue

    let rec dp mask u =
        if mask = (1 <<< n) - 1 then
            graph.[u, 0]
        else if memo.[u, mask] <> Int32.MinValue then
            memo.[u, mask]
        else
            let mutable result = Int32.MinValue

            for v = 0 to n - 1 do
                if (mask &&& (1 <<< v)) = 0 then
                    result <- max result (graph.[u, v] + dp (mask ||| (1 <<< v)) v)

            memo.[u, mask] <- result
            result

    dp 1 0


let parseToInstruction (xs: string array) =
    { person = xs[0]
      neigboor = xs[3]
      happiness = (int xs[2]) * (if xs[1] = "lose" then -1 else 1) }

let buildGraph instructions =
    let nameMapping =
        instructions
        |> Seq.map (fun i -> i.person)
        |> Seq.distinct
        |> Seq.indexed
        |> Seq.map (fun (i, s) -> s, i)
        |> Map.ofSeq

    let folder (g: int [,]) x =
        let pi = nameMapping[x.person]
        let ni = nameMapping[x.neigboor]

        let v =
            if g[pi, ni] = Int32.MinValue then
                x.happiness
            else
                g[pi, ni] + x.happiness

        g[pi, ni] <- v
        g[ni, pi] <- v
        g

    let initalGraph =
        Array2D.create<int> nameMapping.Count nameMapping.Count Int32.MinValue

    instructions |> Seq.fold folder initalGraph

[<Theory>]
[<InlineData(330, "../../../Solutions/13/data_test.txt")>]
[<InlineData(733, "../../../Solutions/13/data.txt")>]
let solve1 expected input =
    input
    |> parseEachLine (withRegex "^(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.$")
    |> Seq.map parseToInstruction
    |> buildGraph
    |> heldKarp
    |> should equal expected

let addMe (g: int [,]) =
    let l = Array2D.length1 g
    let newGraph = Array2D.create<int> (l + 1) (l + 1) 0
    for i in [ 0 .. l-1 ] do
        for j in [0 .. l-1] do
            newGraph[i,j] <- g[i,j]

    newGraph[l, l] <- Int32.MinValue
    newGraph

[<Theory>]
[<InlineData(725, "../../../Solutions/13/data.txt")>]
let solve2 expected input =
    input
    |> parseEachLine (withRegex "^(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.$")
    |> Seq.map parseToInstruction
    |> buildGraph
    |> addMe
    |> heldKarp
    |> should equal expected
