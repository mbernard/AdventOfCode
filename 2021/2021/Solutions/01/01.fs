module _01

open Common
open Xunit

let solve1 path =
  parseEachLine asInt path
  |> Seq.fold (fun (count:int, prev:int) e ->
    let c = if e > prev then count + 1 else count
    (c, e)) (0, System.Int32.MaxValue)
  |> fst

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/01/data.txt"
    Assert.Equal(1393, res)
