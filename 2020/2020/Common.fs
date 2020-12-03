module Common

open System.IO
open System
open System.Text.RegularExpressions

// helper methods for parsing
let parseFirstLine f (fileName : string) =
    use fs = new FileStream(fileName, FileMode.Open)
    use reader = new StreamReader(fs)
    f (reader.ReadLine())

let parseEachLine f = File.ReadLines >> Seq.map f
let parseEachLineIndexed f = File.ReadLines >> Seq.mapi f

let asString : string -> string = id
let asInt : string -> int = int
let asStringArray : string [] -> string [] = Array.map string
let asCharArray (x:string) = x.ToCharArray()
let asIntArray : string [] -> int [] = Array.map int
let asInt64Array : string [] -> int64 [] = Array.map int64
let splitBy (c : string) f (str : string) = str.Split([| c |], StringSplitOptions.None) |> f
let extractInts str = [| for m in Regex.Matches(str, "(-?\d+)") -> int m.Value |]
let withRegex regex str = [| for m in Regex.Match(str, regex).Groups -> m.Value|] |> Array.tail
let charsToStr (chars : char seq) = chars |> Seq.map string |> String.concat ""