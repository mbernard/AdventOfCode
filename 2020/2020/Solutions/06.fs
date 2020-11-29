module _06

open Common

let rec countOrbit (parents: (string * string) list) map (k,v) (root:string) =
    if v = root then
        parents
    else
        let parent = map |> Map.pick (fun x y -> if x = v then Some (x,y) else None)
        countOrbit (parent::parents) map parent root

let getParents (map, parents, root) k v =
    (map, parents @ countOrbit [(k,v)] map (k,v) root, root)

let solve (input:seq<string[]>) =
    let (_, parents, root) =
        input
        |> Seq.map (fun x -> (x.[1], x.[0]))
        |> Map.ofSeq
        |> (fun x -> Map.fold getParents (x, [], "COM") x)
    List.length parents

let solve1 =
    "../../../Data/06.txt"
    |> parseEachLine (splitBy ")" asStringArray)
    |> solve

let getKey map x = map |> Map.pick (fun y z -> if y = x then Some (y,z) else None)


let findCommonParents map =
    let you = getKey map "YOU"
    let san = getKey map "SAN"

    let (_, youp, _) = getParents (map, [], "COM") (fst you) (snd you)
    let (_, sanp, _) = getParents (map, [], "COM") (fst san) (snd san)
    
    Set.intersect (youp |> Set.ofList) (sanp |> Set.ofList)
    |> Set.toList

let distance map x =
    let you = getKey map "YOU"
    let san = getKey map "SAN"

    let (_, youp, _) = getParents (map, [], fst x) (fst you) (snd you)
    let (_, sanp, _) = getParents (map, [], fst x) (fst san) (snd san)

    List.length youp + List.length sanp

let solve2 data =
    findCommonParents data
    |> List.map (distance data)
    |> List.min
    |> (-) 2
    |> abs
    

open Xunit


[<Fact>]
let ``test`` () =
    let data =
        seq {
            yield [|"COM" ; "B" |]
            yield [|"B" ; "C" |]
            yield [|"C" ; "D" |]
            yield [|"D" ; "E" |]
            yield [|"E" ; "F" |]
            yield [|"B" ; "G" |]
            yield [|"G" ; "H" |]
            yield [|"D" ; "I" |]
            yield [|"E" ; "J" |]
            yield [|"J" ; "K" |]
            yield [|"K" ; "L" |]
            yield [|"K" ; "YOU" |]
            yield [|"I" ; "SAN" |]
        }
        |> Seq.map (fun x -> (x.[1], x.[0]))
        |> Map.ofSeq

    let actual = solve2 data 
    Assert.Equal(4, actual)

[<Fact>]
let ``test2`` () =
    let data =
        seq {
            yield [|"COM" ; "B" |]
            yield [|"B" ; "C" |]
            yield [|"C" ; "D" |]
            yield [|"D" ; "E" |]
            yield [|"E" ; "F" |]
            yield [|"B" ; "G" |]
            yield [|"G" ; "H" |]
            yield [|"D" ; "I" |]
            yield [|"E" ; "J" |]
            yield [|"J" ; "K" |]
            yield [|"K" ; "L" |]
        }
    let actual = solve data 
    Assert.Equal(42, actual)

[<Fact>]
let ``solve 1`` () =
    let actual = solve1
    Assert.Equal(154386, actual)

[<Fact>]
let ``solve 2`` () =
    let actual = 
        "../../../Data/06.txt"
        |> parseEachLine (splitBy ")" asStringArray)
        |> Seq.map (fun x -> (x.[1], x.[0]))
        |> Map.ofSeq
        |> solve2
    Assert.Equal(346, actual)
