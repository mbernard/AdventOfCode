module Tests01

open Xunit
open _01

[<Fact>]
let ``Test solve2``() = solve2

[<Theory>]
[<InlineData(14, 2)>]
[<InlineData(1969, 966)>]
[<InlineData(100756, 50346)>]
let ``getFuel2 test`` input expected =
    let actual = getFuel2 input
    Assert.Equal(expected, actual)
