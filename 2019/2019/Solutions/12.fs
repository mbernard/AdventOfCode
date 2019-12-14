module _12

open Common
open Xunit

type Coord = {X: int; Y:int; Z:int }
type Moon = { Name: int; Position : Coord; Velocity: Coord }

let initialize i (p: int array) = 
    { Name = i; Position = { X = p.[0]; Y = p.[1]; Z = p.[2]}; Velocity = {X = 0; Y = 0; Z = 0} }

let caculateVelocity s (x,y) =
    if x.Position.X < y.Position.X 
    then {}
    else
    s

let updateVelocity moons = 
    moons
    |> Seq.allPairs
    |> Seq.fold calculateVelocity moons
    ()

let updatePosition = ()
let execute moons x =
    
    moons

[<Fact>]
let ``Solve 1`` () =
    let moons = 
        "../../../Data/12.txt"
        |> parseEachLine extractInts
        |> Seq.mapi initialize

    let actual = 
        [1..1000]
        |> List.fold execute moons

    ()