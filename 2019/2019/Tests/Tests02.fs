module Tests02

open _02
open Xunit

[<Fact>]
let ``read next instruction`` () =
    let expected = [|2;0;0;0;99|]
    let actual = readNextInstruction 0 [|1;0;0;0;99|]
    Assert.Equal<int>(expected, actual)

[<Fact>]
let ``read next instruction 2`` () =
    let input = [|2;3;0;3;99|]
    let expected = [|2;3;0;6;99|]
    let actual = 
        input
        |> readNextInstruction 0
    Assert.Equal<int>(expected, actual)

[<Fact>]
let ``read next instruction 3`` () =
    let input = [|2;4;4;5;99;0|]
    let expected = [|2;4;4;5;99;9801|]
    let actual = 
        input
        |> readNextInstruction 0
    Assert.Equal<int>(expected, actual)

[<Fact>]
let ``read next instruction 4`` () =
    let input = [|1;1;1;4;99;5;6;0;99|]
    let expected = [|30;1;1;4;2;5;6;0;99|]
    let actual = 
        input
        |> readNextInstruction 0
    Assert.Equal<int>(expected, actual)

[<Fact>]
let ``Test solve1`` () =
    let actual = solve1
    Assert.Equal<int>(3716293, actual)

[<Fact>]
let ``Test solve2`` () =
    let actual = solve2 19690720
    Assert.Equal((64,29), actual)