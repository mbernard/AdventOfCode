module _04

open System
open Xunit
open Common

//byr (Birth Year)
//iyr (Issue Year)
//eyr (Expiration Year)
//hgt (Height)
//hcl (Hair Color)
//ecl (Eye Color)
//pid (Passport ID)
//cid (Country ID)

type Passport =
    { BirthYear: string option
      IssueYear: string option
      ExpirationYear: string option
      Height: string option
      HairColor: string option
      EyeColor: string option
      PassportId: string option
      CountryId: string option }

//let addToPassport x s =
//    match s
//
//let parsePassport x s =
//    x with
//
//
//let parsePassports (xs: Passport list, current: Passport option) (x: string) =
//    let passport =
//        current
//        |> Option.defaultValue {}
//
//    if x = "" then
//        (passport::xs, None)
//    else
//        (xs, None)

let assemble (xs: string list, current: string option) (x: string) =
    if x = "" then
        let c = current |> Option.get
        (c :: xs, None)
    else
        let c =
            match current with
            | None -> x
            | Some s -> $"{current} {x}"
            |> Some

        (xs, c)

let isValid (x: string) =
    x.Contains("byr:")
    && x.Contains("iyr:")
    && x.Contains("eyr:")
    && x.Contains("hgt:")
    && x.Contains("hcl:")
    && x.Contains("ecl:")
    && x.Contains("pid:")

let solve1 data =
    data
    |> parseEachLine asString
    |> Seq.fold assemble ([], None)
    |> fst
    |> List.filter isValid
    |> List.length

[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/04/data.txt"
    Assert.Equal(237, res)

[<Fact>]
let ``Solve 1 - example 1`` () =
    let res =
        solve1 "../../../Solutions/04/data-test-1.txt"
    Assert.Equal(2, res)
