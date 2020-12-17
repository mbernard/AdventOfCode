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
            (rs.[0] |> int, rs.[1] |> int))
        |> Array.toList

    { Name = gs.[1]; Ranges = ranges }

let rec parseRules rules (l :: lines) =
    if l = String.Empty then
        (rules, lines)
    else
        let r = parseRule l
        parseRules (r :: rules) lines

let rec parseTickets tickets (l :: lines) =
    if l = String.Empty then
        (tickets, lines)
    else
        let t =
            l.Split(',') |> Array.toList |> List.map int

        if lines = [] then ((t :: tickets), lines) else parseTickets (t :: tickets) lines

let parse (lines: string list) =
    let (rules, rest) = parseRules [] lines
    let (myTicket :: _, rest2) = parseTickets [] (rest |> List.tail) // discard the useless line
    let (tickets, _) = parseTickets [] (rest2 |> List.tail) // discard the useless line

    { Rules = rules
      YourTicket = myTicket
      OtherTickets = tickets }

let inRange x r = fst r <= x && x <= snd r

let errorRate (rules: Rule list) sum ticketValues =
    let allRanges =
        rules |> List.fold (fun s x -> x.Ranges @ s) []

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

let discardInvalid data =
    let allRanges =
        data.Rules
        |> List.fold (fun s x -> x.Ranges @ s) []

    let validTickets =
        data.OtherTickets
        |> List.filter (fun y ->
            y
            |> List.forall (fun z -> allRanges |> List.exists (inRange z)))

    { data with
          OtherTickets = validTickets }

let findRuleIndices (xs: int [] []) r =
    let i =
        xs
        |> Array.mapi (fun i ruleValues ->
            ruleValues
            |> Array.forall (fun v -> r.Ranges |> List.exists (inRange v)))

    (r.Name, i)
    
let isValidRuleForValues (values:int []) (ranges:Range list) : bool =
    values
    |> Array.forall (fun v -> ranges |> List.exists (inRange v))
//    
//    ranges
//    |> List.exists (fun r -> values |> Array.forall (fun x -> inRange x r))
    
let findRules data i values =
    let rs =  
        data.Rules
        |> List.filter (fun x -> isValidRuleForValues values x.Ranges)
    
    (i,rs)

let mapRules data =
    let ts =
        Array2D.init (data.OtherTickets |> List.length) (data.Rules |> List.length) (fun i j ->
            data.OtherTickets |> List.item i |> List.item j)

    let x =
        seq {
            for i in [ 0 .. (ts |> Array2D.length2) - 1 ] do
                yield ts.[*, i]
        }
        |> Seq.toArray
    x
    |> Array.mapi (findRules data)
    |> List.ofArray


let sumDirectionRules data (mappedRules: (int * Rule) list) =
    let x =
        mappedRules
        |> List.sortBy fst
        |> (fun x -> x)
        |> List.map snd
        |> List.zip data.YourTicket
        |> List.filter (fun (x, y) -> y.Name.StartsWith("departure"))
    x
    |> List.map (fst>>int64)
    |> List.fold (*) 1L
    
let rec solveRules (xs:(int * Rule list)list) =
    let solved =
        xs
        |> List.filter (fun (i,rules) -> rules |> List.length = 1)
        |> List.fold (fun s (i,x) -> x @ s) []
    if xs.Length = List.length solved  then xs
    else
        solveRules (xs
        |> List.map (fun (i,x) ->
            if x |> List.length = 1 then (i,x)
            else (i, List.except solved x)))

let solve2 data =
    let d = 
        data
        |> parseEachLine asString
        |> Seq.toList
        |> parse
        |> discardInvalid
    d
    |> mapRules
    |> solveRules
    |> List.map (fun (i,x::xs) -> (i,x))
    |> sumDirectionRules d


[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/16/data.txt"
    Assert.Equal(1093427331937L, res)

[<Fact>]
let ``Solve 2 - example 2`` () =
    let res =
        solve2 "../../../Solutions/16/data-test-2.txt"

    Assert.Equal(1L, res)
