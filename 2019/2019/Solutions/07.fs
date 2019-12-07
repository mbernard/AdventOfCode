module _07

open Common

// All ordered picks {x_i1, x_i2, .. , x_ik} of k out of n elements {x_1,..,x_n}
// where i1 < i2 < .. < ik
let picks n L =
    let rec aux nleft acc L =
        seq {
            match nleft, L with
            | 0, _ -> yield acc
            | _, [] -> ()
            | nleft, h :: t ->
                yield! aux (nleft - 1) (h :: acc) t
                yield! aux nleft acc t
        }
    aux n [] L

// Distribute an element y over a list:
// {x1,..,xn} --> {y,x1,..,xn}, {x1,y,x2,..,xn}, .. , {x1,..,xn,y}
let distrib y L =
    let rec aux pre post =
        seq {
            match post with
            | [] -> yield (L @ [ y ])
            | h :: t ->
                yield (pre @ y :: post)
                yield! aux (pre @ [ h ]) t
        }
    aux [] L

// All permutations of a single list = the head of a list distributed
// over all permutations of its tail
let rec getAllPerms =
    function
    | [] -> Seq.singleton []
    | h :: t -> getAllPerms t |> Seq.collect (distrib h)

// All k-element permutations out of n elements =
// all permutations of all ordered picks of length k combined
let getPerms2 n lst = picks n lst |> Seq.collect getAllPerms

// Generates the cartesian outer product of a list of sequences LL
let rec outerProduct =
    function
    | [] -> Seq.singleton []
    | L :: Ls -> L |> Seq.collect (fun x -> outerProduct Ls |> Seq.map (fun L -> x :: L))

// Generates all n-element combination from a list L
let getPermsWithRep2 n L = List.replicate n L |> outerProduct

open _05
open FSharpx.Collections

let execute (s, program) x =
    let input = 
        s
        |> Queue.toSeq
        |> Seq.cons x
        |> Queue.ofSeq

    (readNextInstruction 0 input Queue.empty (program |> Array.copy)
     |> fst, program)

let runSeq program seq =
    let input = Queue.ofList [0]
    seq
    |> Seq.fold execute (input, program)
    |> (fun x -> (seq, fst x |> Queue.ofSeq |> Seq.head))

let solve program =
    getPerms2 5 [ 0 .. 4 ]
    |> Seq.map (runSeq program)
    |> Seq.maxBy snd
    |> snd

open Xunit

let testCases: obj array seq =
    seq {
        yield [| [| 3; 15; 3; 16; 1002; 16; 10; 16; 1; 16; 15; 15; 4; 15; 99; 0; 0 |]
                 [| 4; 3; 2; 1; 0 |] 
                 43210 |]
        yield [| [| 3; 23; 3; 24; 1002; 24; 10; 24; 1002; 23; -1; 23; 101; 5; 23; 23; 1; 24; 23; 23; 4; 23; 99; 0; 0 |]
                 [| 0; 1; 2; 3; 4 |] 
                 54321 |]
        yield [| [| 3; 31; 3; 32; 1002; 32; 10; 32; 1001; 31; -2; 31; 1007; 31; 0; 33; 1002; 33; 7; 33; 1; 33; 31; 31; 1; 32; 31; 31; 4; 31; 99; 0; 0; 0 |]
                 [| 1; 0; 4; 3; 2 |] 
                 65210 |]
    }

[<Fact>]
let ``Solve 1`` () =
    let actual = 
        "../../../Data/07.txt"
        |> parseFirstLine (splitBy "," asIntArray)
        |> solve
    Assert.Equal(0, actual)

[<Theory>]
[<MemberData("testCases")>]
let ``test run seq`` program x expected =
    let actual = runSeq program (x |> Array.toSeq) |> snd
    Assert.Equal(expected, actual)

[<Theory>]
[<MemberData("testCases")>]
let ``solve test`` program x expected =
    let actual = solve program
    Assert.Equal(expected, actual)
