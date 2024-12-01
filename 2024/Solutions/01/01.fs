module day01

open System
open Xunit
open Common
open FsUnit


let parse path =
    path
    |> parseEachLine (withRegex "(\d+).*?(\d+)")
    |> Seq.map (fun x -> int x.[0], int x.[1])

let buildLists (xs,ys) (x,y) = x :: xs, y :: ys
let sortLists (xs,ys) = xs |> List.sort, ys |> List.sort
let diff (x,y) = abs (x - y)

let solve1 input =
    Seq.fold buildLists ([], []) input
    |> sortLists
    |> (fun (x,y) -> List.zip x y)
    |> List.map diff
    |> List.sum

[<Fact>]
let solve1_test_data () =
    [ (3, 4)
      (4, 3)
      (2, 5)
      (1, 3)
      (3, 9)
      (3, 3) ]
      |> solve1
      |> should equal 11

[<Fact>]
let solve1_test () =
    parse "../../../Solutions/01/data.txt"
    |> solve1
    |> should equal 2057374

let score (s,xs: Map<int,int>) x = 
    xs
    |> Map.tryFind x
    |> Option.defaultValue 0
    |> (fun value -> (s + (x*value), xs))
    

    
let solve2 input =
   let xs, ys = Seq.fold buildLists ([], []) input
   let counts = ys |> List.countBy id |> Map.ofList
   xs
   |> List.fold score (0,counts)
   |> fst
   

[<Fact>]
let solve2_test_data () =
    [ (3, 4)
      (4, 3)
      (2, 5)
      (1, 3)
      (3, 9)
      (3, 3) ]
      |> solve2
      |> should equal 31

[<Fact>]
let solve2_test () = 
    parse "../../../Solutions/01/data.txt"
    |> solve2
    |> should equal 23177084
