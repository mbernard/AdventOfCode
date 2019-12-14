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
        |> List.filter (fun x -> fst x = m.Name)
        |> List.map snd
    myv
    |> List.fold applyVelocity m

let updateVelocity moons = 
    let vs =
        moons
        |> List.allPairs moons
        |> List.map calculateVelocity
    moons
    |> List.map (applyVelocities vs)

let updatePosition m =
    { m with Position = 
        { X = m.Position.X + m.Velocity.X;
            Y = m.Position.Y + m.Velocity.Y;
            Z = m.Position.Z + m.Velocity.Z; } }

let execute moons i =
    moons
    |> updateVelocity
    |> List.map updatePosition

let calculateTotalEnergy e m =
    let pot = abs m.Position.X + abs m.Position.Y + abs m.Position.Z
    let kin = abs m.Velocity.X + abs m.Velocity.Y + abs m.Velocity.Z
    pot * kin
    |> (+) e

let getVelX x = x.Velocity.X
let getVelY x = x.Velocity.Y
let getVelZ x = x.Velocity.Z

let rec gcd a b =
    if b = 0
        then abs a
    else gcd b (a % b)

let lcmSimple a b = a*b/(gcd a b)

let rec lcm = function
    | [a;b] -> lcmSimple a b
    | head::tail -> lcmSimple (head) (lcm (tail))
    | [] -> 1

let stepsToInitialState xs xi x =
    
    let rec exec ms i f =
        let ms = execute ms i
        if List.item xi ms |> f = 0 
        then i
        else exec ms (i+1) f

    [getVelX; getVelY; getVelZ]
    |> List.map (exec xs 1)

[<Theory>]
[<InlineData(2772, "../../../Data/12_test1.txt")>]
[<InlineData(4686774924L, "../../../Data/12_test2.txt")>]
[<InlineData(0, "../../../Data/12.txt")>]
let ``solve 2 test`` expected file =
    let moons = 
        file
        |> parseEachLine extractInts
        |> Seq.mapi initialize
        |> List.ofSeq
    let steps = 
        moons
        |> List.mapi (stepsToInitialState moons)
        |> List.concat
        |> lcm

    Assert.Equal(expected, steps)

[<Theory>]
[<InlineData(179, 10, "../../../Data/12_test1.txt")>]
[<InlineData(12070, 1000, "../../../Data/12.txt")>]
let ``Solve 1`` expected i file =
    let moons = 
        file
        |> parseEachLine extractInts
        |> Seq.mapi initialize
        |> List.ofSeq

    let actual = 
        [1..i]
        |> List.fold execute moons
        |> List.fold calculateTotalEnergy 0

    Assert.Equal(expected, actual)