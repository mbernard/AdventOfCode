module _12

open Common
open Xunit

type Coord = {X: int; Y:int; Z:int }
type Moon = { Name: int; Position : Coord; Velocity: Coord }

let initialize i (p: int array) = 
    { Name = i; Position = { X = p.[0]; Y = p.[1]; Z = p.[2]}; Velocity = {X = 0; Y = 0; Z = 0} }

let getX x = x.Position.X
let getY x = x.Position.Y
let getZ x = x.Position.Z

let comparePosition x y f =
    if f x = f y
    then 0
    else if f x > f y
    then -1
    else 1

let calculateVelocity (x,y) =
    (x.Name,
     { X = comparePosition x y getX 
       Y = comparePosition x y getY
       Z = comparePosition x y getZ })

let applyVelocity m v =
    { m with Velocity = { X = m.Velocity.X + v.X; Y = m.Velocity.Y + v.Y; Z = m.Velocity.Z + v.Z;}}

let applyVelocities vs m =
    let myv =
        vs
        |> Array.filter (fun x -> fst x = m.Name)
        |> Array.map snd
    myv
    |> Array.fold applyVelocity m

let updateVelocity moons = 
    let vs =
        moons
        |> Array.allPairs moons
        |> Array.map calculateVelocity
    moons
    |> Array.map (applyVelocities vs)

let updatePosition m =
    { m with Position = 
        { X = m.Position.X + m.Velocity.X;
            Y = m.Position.Y + m.Velocity.Y;
            Z = m.Position.Z + m.Velocity.Z; } }

let execute moons i =
    moons
    |> updateVelocity
    |> Array.map updatePosition

let calculateTotalEnergy e m =
    let pot = abs m.Position.X + abs m.Position.Y + abs m.Position.Z
    let kin = abs m.Velocity.X + abs m.Velocity.Y + abs m.Velocity.Z
    pot * kin
    |> (+) e

let getVelX x = x.Velocity.X
let getVelY x = x.Velocity.Y
let getVelZ x = x.Velocity.Z
let getXAxis x = x.Position.X,x.Velocity.X
let getYAxis x = x.Position.Y,x.Velocity.Y
let getZAxis x = x.Position.Z,x.Velocity.Z

let rec gcd x y = if y = 0L then abs x else gcd y (x % y)

let lcm x y = x * y / (gcd x y)

let stepsToInitialState2 initialStates f =
    let initialStateOnAxis = Array.map f initialStates
    let rec exec ms i  =
        let ms = execute ms i
        let state = Array.map f ms
        if  state = initialStateOnAxis
        then i
        else exec ms (i+1L)

    exec initialStates 1L


let stepsToInitialState xs xi x =
    let rec exec ms i =
        let ms = execute ms i
        if ms.[xi] = x 
        then i
        else exec ms (i+1L)
    exec xs 1L

[<Theory>]
[<InlineData(2772L, "../../../Data/12_test1.txt")>]
[<InlineData(4686774924L, "../../../Data/12_test2.txt")>]
[<InlineData(500903629351944L, "../../../Data/12.txt")>]
let ``solve 2 test`` expected file =
    let moons = 
        file
        |> parseEachLine extractInts
        |> Seq.mapi initialize
        |> Array.ofSeq
    let steps =
        [|getXAxis; getYAxis; getZAxis|]
        |> Array.map (stepsToInitialState2 moons)
        |> Array.distinct
    let actual = 
        steps
        |> Array.reduce (fun a b -> lcm a b)

    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(179, 10, "../../../Data/12_test1.txt")>]
[<InlineData(12070, 1000, "../../../Data/12.txt")>]
let ``Solve 1`` expected i file =
    let moons = 
        file
        |> parseEachLine extractInts
        |> Seq.mapi initialize
        |> Array.ofSeq

    let actual = 
        [|1..i|]
        |> Array.fold execute moons
        |> Array.fold calculateTotalEnergy 0

    Assert.Equal(expected, actual)