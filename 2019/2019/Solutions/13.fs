module _13

open Common
open Computer
open Xunit

let runAndRead computer = computer |> executeUntilOutput |> tryReadFromOutput
let runAndReadAcc (s,computer) x =
    let c,x = runAndRead computer
    match x with
    | Some i -> (i::s,c)
    | None -> (s, c)

let runAndReadX i computer = [ 1 .. i ] |> List.fold runAndReadAcc ([], computer)

let rec execute map computer =
    let xs, c = runAndReadX 3 computer
    match c.State with
       | Done -> map
       | Executing -> 
            let o::y::x::_ = xs
            let m = Map.add (x,y) o map
            execute m c

let rec execute2 map score computer =
    let xs, c = 
        if Map.count map > 1080
        then computer |> writeInput -1L
        else computer
        |> runAndReadX 3
    match c.State with
       | Done -> score
       | Executing -> 
            let o::y::x::_ = xs
            let m = Map.add (x,y) o map
            if x = -1L && y = 0L then execute2 m (score + o) c
            else execute2 m score c

[<Fact>]
let ``Solve 2`` () =
    let actual =
        "../../../Data/13.txt"
        |> parseFirstLine (splitBy "," asInt64Array)
        |> (fun x -> 
            x.[0] <- 2L
            x)
        |> initialize64
        |> execute2 Map.empty 0L

    Assert.Equal(-1L, actual)

[<Fact>]
let ``Solve 1`` () =
    let actual =
        "../../../Data/13.txt"
        |> parseFirstLine (splitBy "," asInt64Array)
        |> initialize64
        |> execute Map.empty
        |> Map.filter (fun k v -> v = 2L)
        |> Map.count
    Assert.Equal(462, actual)