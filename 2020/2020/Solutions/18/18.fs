module _18

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open Xunit
open Common

type Computer =
    { OriginalString: string
      Operators: Stack<char>
      Operands: Stack<int64> }

let removeSpaces (x: string) = x.Replace(" ", "")

let compute computer x =
    if computer.Operators.Count = 0 then
        computer.Operands.Push x
    else
        let res =
            match computer.Operators.Pop() with
            | '+' -> computer.Operands.Pop() + x
            | '*' -> computer.Operands.Pop() * x
            | '(' -> x

        computer.Operands.Push res

    computer

let solveExpression computer x =
    match x with
    | '+'
    | '*'
    | '(' ->
        computer.Operators.Push(x)
        computer
    | ')' -> compute computer (computer.Operands.Pop())
    | ' ' -> computer
    | x -> x |> string |> int64 |> compute computer

let solve1 data =
    data
    |> parseEachLine asString
    |> Seq.toList
    |> List.map
        (removeSpaces
         >> asCharArray
         >> Array.fold
             solveExpression
                { OriginalString = ""
                  Operators = new Stack<char>()
                  Operands = new Stack<int64>() })
    |> List.map (fun x -> x.Operands.Pop())
    |> List.sum


[<Fact>]
let ``Solve 1`` () =
    let res = solve1 "../../../Solutions/18/data.txt"
    Assert.Equal(12918250417632L, res)

[<Theory>]
[<InlineData(26L, "2 * 3 + (4 * 5)")>]
[<InlineData(437L, "5 + (8 * 3 + 9 + 3 * 4 * 3)")>]
[<InlineData(12240L, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")>]
[<InlineData(13632L, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")>]
let ``Solve 1 - example 1`` expected input =
    let res =
        input
        |> asCharArray
        |> Array.fold
            solveExpression
               { OriginalString = ""
                 Operators = new Stack<char>()
                 Operands = new Stack<int64>() }

    Assert.Equal(expected, res.Operands.Pop())

let rec solveParenthesis computer =
    let op = computer.Operators.Pop()

    if op = '(' then
        if computer.Operators.Count > 0 then
            let op2 = computer.Operators.Pop()
            if op2 = '+' then computer.Operands.Pop() + computer.Operands.Pop() |> computer.Operands.Push
            else computer.Operators.Push(op2)
        
        computer
    else
        let r =
            match op with
            | '+' -> computer.Operands.Pop() + computer.Operands.Pop()
            | '*' -> computer.Operands.Pop() * computer.Operands.Pop()

        computer.Operands.Push r
        solveParenthesis computer

let compute2 computer x =
    if computer.Operators.Count = 0 then
        computer.Operands.Push x
    else
        let res =
            match computer.Operators.Pop() with
            | '+' -> computer.Operands.Pop() + x
            | '*' ->
                computer.Operators.Push '*' // re-stack for later
                x
            | '(' ->
                computer.Operators.Push '(' // re-stack for later
                x

        computer.Operands.Push res

    computer

let solveExpression2 computer x =
    match x with
    | '+'
    | '*'
    | '(' ->
        computer.Operators.Push(x)
        computer
    | ')' -> solveParenthesis computer
    | ' ' -> computer
    | x -> x |> string |> int64 |> compute2 computer

let rec go computer =
    if computer.Operators.Count = 0 then
        computer
    else
        let r =
            match computer.Operators.Pop() with
            | '+' -> computer.Operands.Pop() + computer.Operands.Pop()
            | '*' -> computer.Operands.Pop() * computer.Operands.Pop()

        computer.Operands.Push r
        go computer

let solve2 data =
    data
    |> parseEachLine asString
    |> Seq.toList
    |> List.map
        (fun x ->
             x
             |> asCharArray
             |> Array.fold
                 solveExpression2
                    { OriginalString = x
                      Operators = new Stack<char>()
                      Operands = new Stack<int64>() }
             |> go)
    |> List.map (fun x ->
        let y = x.Operands.Pop()
        if x.Operands.Count = 0 then y else failwith "boom")
    |> List.sum

[<Fact>]
let ``Solve 2`` () =
    let res = solve2 "../../../Solutions/18/data.txt"
    Assert.Equal(171259538712010L, res)

[<Theory>]
[<InlineData(46L, "2 * 3 + (4 * 5)")>]
[<InlineData(1445L, "5 + (8 * 3 + 9 + 3 * 4 * 3)")>]
[<InlineData(669060L, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")>]
[<InlineData(23340L, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")>]
[<InlineData(51L, "1 + (2 * 3) + (4 * (5 + 6))")>]
[<InlineData(63768L, "8 * 6 + (5 * (3 + 2 * 8 + 2 + 7 + 4) + (7 + 5 * 6) * 9)")>]
let ``Solve 2 - example 1`` expected input =
    let res =
        input
        |> asCharArray
        |> Array.fold
            solveExpression2
               { OriginalString = input
                 Operators = new Stack<char>()
                 Operands = new Stack<int64>() }
        |> go

    Assert.Equal(expected, res.Operands.Pop())
