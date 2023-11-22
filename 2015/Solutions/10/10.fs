module day10

open System
open Xunit
open Common
open FsUnit

let rec lookAndSayChars acc rest = 
    match rest with
    | [] -> acc
    | x :: xs -> 
        lookAndSayChars 
            ((x :: (xs |> List.takeWhile ((=) x)))::acc)
            (xs |> List.skipWhile ((=) x))

let memoize f =
    let cache = ref Map.empty
    fun x ->
        match (cache.Value).TryFind(x) with
        | Some res -> res
        | None ->
                let res = f x
                cache.Value <- (cache.Value).Add(x,res)
                res

let lookAndSay (x : string) =
    x.ToCharArray()
    |> List.ofArray
    |> ((memoize lookAndSayChars) [])
    |> List.rev
    |> List.map (fun identicals -> (identicals |> List.length, identicals |> List.head))
    |> List.map (fun (nb, char) -> nb.ToString() + char.ToString())
    |> String.concat String.Empty

let repeat f times input =
    {1..times}
    |> Seq.fold (fun s _ -> f s)  input

[<Theory>]
[<InlineDataAttribute(6, "1", 5)>]
[<InlineDataAttribute(360154, "1113122113", 40)>]
[<InlineDataAttribute(5103798, "1113122113", 50)>]
let solve1 (expected, input: string, times) = 
    input
    |> repeat lookAndSay times
    |> String.length
    |> should equal expected
