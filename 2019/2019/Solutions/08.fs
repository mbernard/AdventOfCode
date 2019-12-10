module _08

open Common

let p = parseFirstLine (fun x -> x.ToCharArray() |> Array.map (string >> int)) "../../../Data/08.txt"

let numberOfDigitInLayer n layer =
    layer
    |> Array.countBy id
    |> Array.tryFind (fun (k,v) -> k = n)
    |> function
        | Some (k,v) -> v
        | None -> 0

let solve data wide tall =
    let chunk = Array.length data / (wide * tall)
    data
    |> Array.splitInto chunk
    |> Array.minBy (numberOfDigitInLayer 0)
    |> (fun l -> numberOfDigitInLayer 1 l * numberOfDigitInLayer 2 l)

let initializer (data:int[][]) x y =
    data.[x].[y]

let rec findValue x (xs:int[]) =
    match xs.[x] with
    | 0 -> 0
    | 1 -> 1
    | 2 -> if x = xs.Length - 1 then 2 else findValue (x + 1) xs
    | _ -> failwith "invalid"

let findPixelValue (xs:int [,]) x =
    xs.[*,x..x] 
    |> Seq.cast 
    |> Seq.toArray
    |> findValue 0

let toChar x =
    match x with
    | 0 -> '.'
    | 1 -> '#'
    | _ -> ' '

let printer wide data =
    data
    |> List.iteri (fun i x -> 
        if (i+1) % wide = 0 
        then printfn "%c" (toChar x)
        else printf "%c" (toChar x))

let solve2  =
    let data = p
    let wide = 25
    let tall = 6
    let size = wide * tall
    let chunk = Array.length data / size
    let m = Array2D.init chunk size (initializer (data |> Array.splitInto chunk))
    
    [0..size-1]
    |> List.map (findPixelValue m)
    |> printer wide
    ()


open Xunit

[<Fact>]
let ``Solve 1`` () =
    let actual = solve p 25 6
    Assert.Equal(2318, actual)

[<Theory>]
[<InlineData(1, "123456789012", 3, 2)>]
let ``Solve test`` expected (data:string) wide tall =
    let actual = solve (data.ToCharArray() |> Array.map (string >> int)) wide tall
    Assert.Equal(expected, actual)