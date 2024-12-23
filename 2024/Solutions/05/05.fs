module day05

open System
open Xunit
open Common
open FsUnit

type Manual = {
    Rules: Map<int, Set<int>>
    Updates: int list list
    }

let parse path = 
    let lines = path |> parseEachLine asString |> Array.ofSeq
    let emptyLine = lines |> Seq.findIndex (fun x -> x = String.Empty)
    let parts = lines |> Array.splitAt emptyLine

    let parseRule (m:Map<int, Set<int>>) (x: String) =
        let values = x.Split "|" |> Array.map int
        m.Change (values[0],Option.defaultValue Set.empty >> Set.add values[1] >> Some)

    let rules =
        parts
        |> fst
        |> Array.fold parseRule Map.empty

    let updates =
        parts
        |> snd
        |> List.ofArray
        |> List.tail // skip the empty line
        |> List.map (extractInts >> List.ofArray)

    { Rules = rules; Updates = updates }

type UpdateStatus = Correct | Incorrect
    
let getMiddleValue xs = 
    xs  
    |> List.splitInto 2
    |> List.head
    |> List.last

let getStatus (m: Manual) (report: int list) =
    let rec loop prevPages ps =
        match ps with
        | [] -> Correct
        | x::rest ->
            match m.Rules.TryFind x with
            | None -> loop (Set.add x prevPages) rest
            | Some v -> 
                if v |> Set.intersect prevPages |> Set.isEmpty 
                then loop (Set.add x prevPages) rest
                else Incorrect

    (report, loop Set.empty report)

let solve1 m =
    m.Updates
    |> List.choose (fun report ->
        match getStatus m report with
        | (pages, Correct) -> Some (getMiddleValue pages)
        | _ -> None)
    |> List.sum

[<Fact>]
let solve1_testData () =
    parse "../../../Solutions/05/test_data.txt"
    |> solve1
    |> should equal 143

[<Fact>]
let solve1_test () =
    parse "../../../Solutions/05/data.txt"
    |> solve1
    |> should equal 5762

let reorderPages m report =
    let pages = Set.ofList report
    
    let pagesWithoutRules =
        let relevantKeys = (m.Rules |> Map.keys |> Set.ofSeq)
        Set.difference pages relevantKeys

    // Only keep revelant pages for this update
    let relevantRules =
        m.Rules
        |> Map.filter (fun k _ -> pages |> Set.contains k) // Only keep rules that have pages in the report
        |> Map.map (fun _ v -> 
            pagesWithoutRules
            |> Set.difference (Set.intersect pages v) // Only keep pages that are in the report
        ) 
        
    let rec loop (rs:Map<int, Set<int>>) res =
        match rs with
        | m when Map.isEmpty m -> res // found solution
        | x ->
            let next = x |> Map.findKey (fun _ v -> v |> Set.isEmpty)
            let newRules = 
                rs 
                |> Map.remove next 
                |> Map.map (fun _ v -> Set.remove next v)
            loop newRules (next::res)

    loop relevantRules (pagesWithoutRules |> Set.toList)

let solve2 m =
    m.Updates
    |> List.choose (fun report ->
        match getStatus m report with
        | (pages, Incorrect) -> Some (getMiddleValue (reorderPages m pages))
        | _ -> None)
    |> List.sum

[<Fact>]
let solve2_testData () =
    parse "../../../Solutions/05/test_data.txt"
    |> solve2
    |> should equal 123

[<Fact>]
let solve2_test () =
    parse "../../../Solutions/05/data.txt"
    |> solve2
    |> should equal 4130