module _19

open System
open System.Text.RegularExpressions
open Xunit
open Common

type Data =
    { Rules: Map<int, string>
      Messages: string list }

let isRuleSolved patterns =
    patterns
    |> List.forall (fun y -> Regex.IsMatch(y, @"\d"))

let parseRule (x: string) =
    let r = x.Split(": ")
    let ps = r.[1].Replace("\"", "")

    (r.[0] |> int, ps)

let rec parse data (x :: rest) =
    if x = String.Empty then
        { data with Messages = rest }
    else
        let rule = parseRule x

        parse
            { data with
                  Rules = data.Rules.Add rule }
            rest

let replace (s:string) (i,p) =
    s.Replace(i |> string, p)

let buildStringPattern (s:string) =
    $"({s})"

let rec solveRule n data =
    let p = data.Rules.[n]
    let xs = Regex.Matches(p, @"(\d+)") |> Seq.map (fun g -> g.Value |> int) |> Seq.toList
    if List.length xs = 0 then
        data,p
    else
        data, xs
        |> List.map (fun x -> (x, solveRule x data |> snd))
        |> List.sortByDescending fst
        |> List.fold replace p
        |> buildStringPattern

let howManyMatches (data, x) =
    let p = $"^{x}$".Replace(" ", "")
    data.Messages
    |> List.filter (fun m -> Regex.IsMatch(m, p))
    |> List.length

let solve1 data =
    data
    |> parseEachLine asString
    |> Seq.toList
    |> parse { Rules = Map.empty; Messages = [] }
    |> solveRule 0
    |> howManyMatches


[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/19/data.txt"
    Assert.Equal(198, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/19/data-test-1.txt"

    Assert.Equal(2, res)
