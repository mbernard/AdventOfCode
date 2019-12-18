module _14

open Common
open Xunit
open System

type Chemical = { Number : int; Name:string}
type Formula = { Inputs: Chemical []; Output: Chemical}

let setItem k v m = if v = 0 then Map.remove k m else Map.add k v m
let getItem k m = Map.tryFind k m |> Option.defaultValue 0

type NanoFactory = 
    { Formulas: Map<string, Formula> ; ToMake: Map<string, int>; LeftOvers: Map<string, int>}
    static member Create formulas = { Formulas = formulas; ToMake = Map.empty; LeftOvers = Map.empty}
    static member SetToMake item n nf = { nf with ToMake = setItem item n nf.ToMake }
    static member SetLeftOvers item n nf = { nf with LeftOvers = setItem item n nf.LeftOvers }
    static member Make item n nf =
        let leftOvers = getItem item nf.LeftOvers
        let toMakeTotal = n + (getItem item nf.ToMake)

        nf 
        |> NanoFactory.SetLeftOvers item (if leftOvers >= toMakeTotal then leftOvers - toMakeTotal else 0)
        |> NanoFactory.SetToMake item (if toMakeTotal >= leftOvers then toMakeTotal - leftOvers else 0)


let asChemical s =
    let number,name = splitBy " " (fun x -> x.[0],x.[1]) s
    { Number = int number; Name = name }

let asFormula line =
    let l,r = splitBy " => " (fun x -> x.[0], x.[1]) line
    let output = asChemical r
    let inputs = splitBy ", " (Array.map asChemical) l
    output.Name,{ Inputs = inputs; Output = output}

let ceilDiv a b = Math.Ceiling(float a / float b) |> int

let rec toOre nf = 
    let nextToMake = nf.ToMake |> Map.tryPick (fun k v -> if k <> "ORE" then Some (k,v) else None) 
    match nextToMake with
    | Some (item,amountToMake) ->
        let formula = Map.find item nf.Formulas 
        let formulasToMake = ceilDiv amountToMake formula.Output.Number

        formula.Inputs
        |> Array.fold (fun s x -> NanoFactory.Make x.Name (formulasToMake * x.Number) s) nf
        |> NanoFactory.SetToMake item 0
        |> NanoFactory.SetLeftOvers item (formula.Output.Number * formulasToMake - amountToMake)
        |> toOre
    | None -> nf.ToMake |> Map.find "ORE"

[<Theory>]
[<InlineData(82892753L, "../../../Data/14_test3.txt")>]
[<InlineData(5586022L, "../../../Data/14_test4.txt")>]
[<InlineData(460664L, "../../../Data/14_test5.txt")>]
[<InlineData(0L, "../../../Data/14.txt")>]
let ``solve 2`` expected file =
    let orePerFuel = 
        parseEachLine asFormula file
        |> Map.ofSeq
        |> NanoFactory.Create
        |> NanoFactory.SetToMake "FUEL" 1
        |> toOre
    let actual = 1_000_000_000_000L / (orePerFuel |> int64)

    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(31, "../../../Data/14_test1.txt")>]
[<InlineData(165, "../../../Data/14_test2.txt")>]
[<InlineData(13312 , "../../../Data/14_test3.txt")>]
[<InlineData(180697, "../../../Data/14_test4.txt")>]
[<InlineData(2210736, "../../../Data/14_test5.txt")>]
[<InlineData(374457, "../../../Data/14.txt")>]
let ``solve 1`` expected file =
    let actual = 
        parseEachLine asFormula file
        |> Map.ofSeq
        |> NanoFactory.Create
        |> NanoFactory.SetToMake "FUEL" 1
        |> toOre

    Assert.Equal(expected, actual)