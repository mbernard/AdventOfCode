module _16

open Common
open Xunit
open System

let generatePattern length i  =
    let pattern =
        [| 0;1;0;-1 |]
        |> Array.collect (Array.create (i))

    let pLength = pattern |> Array.length
    seq {
        for i in [0 .. length] do
            yield Array.item (i % pLength) pattern
    }
    |> Array.ofSeq
    |> Array.tail

let applyPattern input x =
    let res =
        Array.zip x input
        |> Array.map (fun (x,y) -> x*y)
        |> Array.sum
        |> string
    res.ToCharArray()
    |> Array.last
    |> string
    |> Int32.Parse


let runPhase patterns input phaseNumber =
    patterns
    |> Array.map (applyPattern input)

let run phase input =
    let length = (input |> Array.length)
    let patterns =
        [|1..length|]
        |> Array.map (generatePattern length)
    [|1..phase|]
    |> Array.fold (runPhase patterns) input
    


[<Theory>]
[<InlineData("01029498", 4, "12345678")>]
[<InlineData("44098263", 100, "59702216318401831752516109671812909117759516365269440231257788008453756734827826476239905226493589006960132456488870290862893703535753691507244120156137802864317330938106688973624124594371608170692569855778498105517439068022388323566624069202753437742801981883473729701426171077277920013824894757938493999640593305172570727136129712787668811072014245905885251704882055908305407719142264325661477825898619802777868961439647723408833957843810111456367464611239017733042717293598871566304020426484700071315257217011872240492395451028872856605576492864646118292500813545747868096046577484535223887886476125746077660705155595199557168004672030769602168262")>]
let ``solve 1`` expected phases (input:string) =
    let actual = 
        input.ToCharArray()
        |> Array.map (string >> Int32.Parse)
        |> run phases
        |> Array.take 8
        |> Array.map string
        |> String.concat ""

    Assert.Equal(expected, actual)
