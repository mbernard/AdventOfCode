module _16

open System
open System.Text.RegularExpressions
open Xunit
open Common

type Range = int * int
type Rule = { Name: string; Ranges: Range list }

type Data =
    { YourTicket: int list
      OtherTickets: int list list
      Rules: Rule list }

let parseRule line =
    let gs =
        Regex.Match(line, @"^(.+): (.+)$").Groups
        |> Seq.map (fun g -> g.Value)
        |> Seq.toArray
    let ranges =
        gs.[2].Split(" or ")
        |> Array.map (fun s ->
            let rs = s.Split('-')
            (rs.[0]|> int, rs.[1] |> int))
        |> Array.toList
    {Name = gs.[1]; Ranges = ranges}
    
let rec parseRules rules (l::lines) =
    if l = String.Empty then (rules,lines)
    else
        let r = parseRule l
        parseRules (r::rules) lines
        
let rec parseTickets tickets (l::lines) =
    if l = String.Empty then (tickets,lines)
    else
        let t = l.Split(',') |> Array.toList |> List.map int
        if lines = [] then ((t::tickets), lines)
        else
            parseTickets (t::tickets) lines
    
let parse (lines:string list) =
    let (rules, rest) = parseRules [] lines
    let (myTicket::_, rest2) = parseTickets [] (rest |> List.tail) // discard the useless line
    let (tickets, _) = parseTickets [] (rest2 |> List.tail) // discard the useless line
    
    {Rules = rules; YourTicket = myTicket; OtherTickets = tickets}
  
let inRange x r = fst r <= x && x <= snd r 
    
let errorRate (rules:Rule list) sum ticketValues =
    let allRanges = rules |> List.fold (fun s x -> x.Ranges @ s) []
    
    ticketValues
    |> List.filter (fun y -> allRanges |> List.exists (inRange y) |> not)
    |> List.sum
    |> (+) sum
    
let computeErrorRate data =
    data.OtherTickets
    |> List.fold (errorRate data.Rules) 0
    
    
let solve1 data =
    data
    |> parseEachLine asString
    |> Seq.toList
    |> parse
    |> computeErrorRate


[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/16/data.txt"
    Assert.Equal(22057, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/16/data-test-1.txt"

    Assert.Equal(71, res)
