module day14

open System
open Xunit
open Common
open FsUnit
    
type Ingredient = {
    name: string
    capacity: int
    durability: int
    flavor: int
    texture: int
    calories: int
}

let toIngredient (xs: string array) =
    {
        name = xs[0]
        capacity = int xs[1]
        durability = int xs[2]
        flavor = int xs[3]
        texture = int xs[4]
        calories = int xs[5]
    }

[<Theory>]
[<InlineData(0, "../../../Solutions/15/data.txt")>]
let solve1 expected input =
    input
    |> parseEachLine (withRegex "^(.*): capacity (\d+), durability (\d+), flavor (\d+), texture (\d+), calories (\d+)$")
    |> Seq.map toIngredient
    
    0