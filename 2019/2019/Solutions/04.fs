module _04

let toIntArray input =
    input.ToString().ToCharArray()
    |> Array.map int

let hasDouble input = 
    input
    |> toIntArray
    |> Array.pairwise
    |> Array.exists (fun (x,y) -> x = y)

let isIncrementing input = 
    input
    |> toIntArray
    |> Array.pairwise
    |> Array.forall (fun (x,y) -> x <= y)

let isValid input = hasDouble input && isIncrementing input

let solve validator = 
    [136760..595730]
    |> List.filter validator
    |> List.length

let solve1 = solve isValid
    
let hasDouble2 input =
    input 
    |> toIntArray
    |> Array.groupBy (fun x -> x)
    |> Array.exists (fun (_, y) -> Array.length y = 2)

let isValid2 input = hasDouble2 input && isIncrementing input

let solve2 = solve isValid2

open Xunit

[<Fact>]
let ``solve 2`` () =
    let actual = solve2
    Assert.Equal(1264, actual);

[<Theory>]
[<InlineData(true, 112233)>]
[<InlineData(false, 123444)>]
[<InlineData(true, 111122)>]
let ``test is valid2`` expected input =
    let actual = isValid2 input
    Assert.Equal(expected, actual)

[<Fact>]
let ``solve 1`` () =
    let actual = solve1
    Assert.Equal(1873, actual);

[<Theory>]
[<InlineData(true, 111111)>]
[<InlineData(false, 223450)>]
[<InlineData(false, 123789)>]
let ``test is valid`` expected input =
    let actual = isValid input
    Assert.Equal(expected, actual)