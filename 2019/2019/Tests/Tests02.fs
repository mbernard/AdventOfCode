module Tests02

open _02
open Xunit
open Computer 
open FSharpx.Collections

let values: obj array seq =
    seq {
        yield [| [| 2; 0; 0; 0; 99 |]
                 [| 1; 0; 0; 0; 99 |] |]
        yield [| [| 2; 3; 0; 6; 99 |]
                 [| 2; 3; 0; 3; 99 |] |]
        yield [| [| 2; 4; 4; 5; 99; 9801 |]
                 [| 2; 4; 4; 5; 99; 0 |] |]
        yield [| [| 30; 1; 1; 4; 2; 5; 6; 0; 99 |]
                 [| 1; 1; 1; 4; 99; 5; 6; 0; 99 |] |]
    }

[<Theory>]
[<MemberData("values")>]
let ``read next instruction`` (expected, input) =
    let actual = 
        Computer.initialize input Queue.empty
        |> executeUntilHalt
        |> (fun x -> x.Memory)
    
    Assert.Equal<int64>(expected |> Array.map int64, actual |> Map.toArray |> Array.map snd)

[<Fact>]
let ``Test solve1``() =
    let actual = solve1
    Assert.Equal<int>(3716293, actual)

[<Fact>]
let ``Test solve2``() =
    let actual = solve2 19690720
    Assert.Equal((64, 29), actual)
