module _04

open System
open System.Text.RegularExpressions
open Newtonsoft.Json.Converters
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
    { BirthYear: string
      IssueYear: string
      ExpirationYear: string
      Height: string
      HairColor: string
      EyeColor: string
      PassportId: string
      CountryId: string option }

let assemble (xs: string list, current: string option) (x: string) =
    if x = "" then
        let c = current |> Option.get
        (c :: xs, None)
    else
        let c =
            match current with
            | None -> x
            | Some s -> $"{s} {x}"
            |> Some

        (xs, c)
        
let addLastItem (xs,x) = (x |> Option.defaultValue "")::xs

let hasRequiredFields (x: string) =
    x.Contains("byr:")
    && x.Contains("iyr:")
    && x.Contains("eyr:")
    && x.Contains("hgt:")
    && x.Contains("hcl:")
    && x.Contains("ecl:")
    && x.Contains("pid:")

let extractValue (x: string) =
    x.Split(':') |> (fun y -> (y.[0], y.[1]))

let toDic (x: string) =
    x.Split(' ')
    |> Array.map extractValue
    |> Map.ofArray

let isBetween low high x =
    let i = x |> int
    i >= low && i <= high

let isValidHeight (x: string) =
    let n = x.Substring(0, x.Length - 2)

    match x.Substring(x.Length - 2) with
    | "in" -> isBetween 59 76 n
    | "cm" -> isBetween 150 193 n
    | _ ->
        sprintf "invalid %A" (x.Substring(x.Length - 2)) |> ignore
        false

let propIsValid k v =
    let res =
        match k with
        | "byr" -> isBetween 1920 2002 v
        | "iyr" -> isBetween 2010 2020 v
        | "eyr" -> isBetween 2020 2030 v
        | "hgt" -> isValidHeight v
        | "hcl" -> Regex.IsMatch(v, @"#([0-9]|[a-f]){6}")
        | "ecl" -> Regex.IsMatch(v, @"(amb|blu|brn|gry|grn|hzl|oth)")
        | "pid" -> Regex.IsMatch(v, @"^[0-9]{9}$")
        | "cid" -> true
        | _ -> failwithf "invalid %s" k

    res

let isValid2 = Map.forall propIsValid

let solve2 data =
    data
    |> parseEachLine asString
    |> Seq.fold assemble ([], None)
    |> addLastItem
    |> List.filter hasRequiredFields
    |> List.map toDic
    |> List.filter isValid2
    |> List.length

[<Theory>]
[<InlineData("byr", "2002")>]
[<InlineData("hgt", "60in")>]
[<InlineData("hgt", "190cm")>]
[<InlineData("hcl", "#123abc")>]
[<InlineData("ecl", "brn")>]
[<InlineData("pid", "000000001")>]
let ``valid prop`` k v =
    Assert.True(propIsValid k v)
    
[<Theory>]
[<InlineData("byr", "2003")>]
[<InlineData("hgt", "190in")>]
[<InlineData("hgt", "190")>]
[<InlineData("hcl", "#123abz")>]
[<InlineData("hcl", "123abc")>]
[<InlineData("ecl", "wat")>]
[<InlineData("pid", "0123456789")>]
let ``invalid prop`` k v =
    Assert.False(propIsValid k v)

[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/04/data.txt"
    Assert.Equal(172, res)

[<Fact>]
let ``Solve 2 - valid passports`` () =
    let res =
        solve2 "../../../Solutions/04/data-test-valid.txt"

    Assert.Equal(4, res)

[<Fact>]
let ``Solve 2 - invalid passports`` () =
    let res =
        solve2 "../../../Solutions/04/data-test-invalid.txt"

    Assert.Equal(0, res)

let solve1 data =
    data
    |> parseEachLine asString
    |> Seq.fold assemble ([], None)
    |> addLastItem
    |> List.filter hasRequiredFields
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
