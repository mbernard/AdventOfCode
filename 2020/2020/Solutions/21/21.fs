module _21

open System
open System.Text.RegularExpressions
open Xunit
open Common

type Food =
    { Ingredients: string list
      Allergens: string list }

let parseFood line =
    let gs =
        Regex
            .Match(line, @"(.+) \(contains (.+)\)")
            .Groups

    { Ingredients = gs.[1].Value.Split(' ') |> Array.toList
      Allergens = gs.[2].Value.Split(", ") |> Array.toList }

let findIngredients foods a =
    let fs =
        foods
        |> List.filter (fun f -> f.Allergens |> List.exists (fun x -> x = a))

    let possibleIngredients =
        fs
        |> List.collect (fun x -> x.Ingredients)
        |> List.countBy id
        |> List.filter (fun (x, i) -> i = List.length fs)
        |> List.map fst

    (a, possibleIngredients)

let rec matchIngredientsAndAllergens map =
    let solvedAllergens =
        map |> Map.filter (fun k v -> List.length v = 1)

    if map = solvedAllergens then
        map
    else
        let unsolvedAllergens =
            map
            |> Map.filter (fun k v -> List.length v > 1)
            |> Map.map (fun k v ->
                v
                |> List.except
                    (solvedAllergens
                     |> Map.toList
                     |> List.map snd
                     |> List.map List.exactlyOne))


        matchIngredientsAndAllergens
            (Map.ofList
                ((Map.toList solvedAllergens)
                 @ (Map.toList unsolvedAllergens)))


let matchAllergens foods =
    let allAllergens =
        foods
        |> List.collect (fun x -> x.Allergens)
        |> List.distinct

    let ingredientsWithAllergens =
        allAllergens
        |> List.map (findIngredients foods)
        |> Map.ofList
        |> matchIngredientsAndAllergens
        |> Map.toList
        |> List.map snd
        |> List.map List.exactlyOne

    foods
    |> List.collect (fun x -> x.Ingredients)
    |> List.filter (fun i -> ingredientsWithAllergens |> List.contains i |> not)
    |> List.length

let solve1 data =
    data
    |> parseEachLine asString
    |> Seq.toList
    |> List.map parseFood
    |> matchAllergens

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/21/data.txt"
    Assert.Equal(2280, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/21/data-test-1.txt"

    Assert.Equal(5, res)
