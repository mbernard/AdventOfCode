module day04

open System
open Xunit
open Common
open FsUnit

let readE x y (xs: char array2d) = try [| for i in 0 .. 3 -> xs[x, y+i] |] |> charsToStr with | _ -> ""
let readW x y (xs: char array2d) = try [| for i in 0 .. 3 -> xs[x, y-i] |] |> charsToStr with | _ -> ""
let readN x y (xs: char array2d) = try [| for i in 0 .. 3 -> xs[x-i, y] |] |> charsToStr with | _ -> ""
let readS x y (xs: char array2d) = try [| for i in 0 .. 3 -> xs[x+i, y] |] |> charsToStr with | _ -> ""
let readSE x y (xs: char array2d) = try [| for i in 0 .. 3 -> xs[x+i, y+i] |] |> charsToStr with | _ -> "" 
let readSW x y (xs: char array2d) = try [| for i in 0 .. 3 -> xs[x+i, y-i] |] |> charsToStr with | _ -> "" 
let readNW x y (xs: char array2d) = try [| for i in 0 .. 3 -> xs[x-i, y-i] |] |> charsToStr with | _ -> "" 
let readNE x y (xs: char array2d) = try [| for i in 0 .. 3 -> xs[x-i, y+i] |] |> charsToStr with | _ -> "" 

let getAllDirection xs x y =
    [
        readE x y xs
        readSE x y xs
        readS x y xs
        readSW x y xs
        readW x y xs
        readNW x y xs
        readN x y xs
        readNE x y xs
    ]
    |> List.filter (fun s -> s = "XMAS")
    |> List.length

let find xs =
    xs
    |> Array2D.mapi (fun x y v -> if v = 'X' then getAllDirection xs x y else 0)
    |> Seq.cast
    |> Seq.reduce (+)

let solve1 lines =
    lines
    |> array2D
    |> find

let testData = 
    "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX".Split "\r\n"
    |> Seq.map asCharArray

let input =
    "../../../Solutions/04/data.txt"
    |> parseEachLine asCharArray

[<Fact>]
let solve1_testData () = 
    testData
    |> solve1
    |> should equal 18

[<Fact>]
let solve1_test () =
    input
    |> solve1
    |> should equal 2390

let isXmas x y (xs: char array2d) =
    let main = try [| for i in 0 .. 2 -> xs[x-1+i, y-1+i] |] |> charsToStr with | _ -> "" 
    let other =try [| for i in 0 .. 2 -> xs[x-1+i, y+1-i] |] |> charsToStr with | _ -> "" 

    (main = "MAS" || main = "SAM") && (other = "MAS" || other = "SAM")

let countXmas xs = 
    let row = Array2D.length1 xs
    let col = Array2D.length2 xs
    xs
    |> Array2D.mapi (fun x y v -> if v = 'A' && x > 0 && y > 0 && x < row - 1 && y < col - 1 && isXmas x y xs then 1 else 0)
    |> Seq.cast
    |> Seq.reduce (+)

let solve2 lines =
    lines
    |> array2D
    |> countXmas

[<Fact>]
let solve2_testData () = 
    testData
    |> solve2
    |> should equal 9

[<Fact>]
let solve2_test () =
    input
    |> solve2
    |> should equal 1809