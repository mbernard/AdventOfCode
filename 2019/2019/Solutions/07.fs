module _07

open Common
open Computer
open FSharpx.Collections

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

// Solve 1
let execute s x =
    { x with Input = x.Input |> Queue.conj s }
    |> executeUntilOutput
    |> (fun y -> y.Output |> Queue.head)

let runSeq program seq =
    seq
    |> Seq.map int64
    |> Seq.map (fun x -> Computer.initialize program |> writeInput x )
    |> Seq.fold execute 0L

let solve program =
    getPerms2 5 [ 0 .. 4 ]
    |> Seq.map (runSeq program)
    |> Seq.max

// Solve 2

let run s x =
    let o =
        match s with
        | Some y -> { x with Input = x.Input |> Queue.conj y }
        | None -> x
        |> Computer.executeUntilOutput
        |> Computer.tryReadFromOutput
    o 

let rec runAmps x amps =
    amps
    |> List.mapFold run x
    |> (fun (clist, res) ->
        if List.forall (fun z -> z.State = Done) clist
        then clist |> List.last |> (fun x -> x.LastSignal)
        else runAmps res clist)

let initializeAmps program sequence =
    sequence
    |> List.map (fun x -> Computer.initialize (Array.append program [|x|]) |> writeInput (int64 x))
    |> runAmps (Some 0L)

let solve2 program =
    getPerms2 5 [ 5 .. 9 ]
    |> Seq.map (initializeAmps program)
    |> Seq.max

open Xunit

let testCases2: obj array seq =
    seq {
        yield [| [| 3; 26; 1001; 26; -4; 26; 3; 27; 1002; 27; 2; 27; 1; 27; 26; 27; 4; 27; 1001; 28; -1; 28; 1005; 28; 6; 99; 0; 0; 5 |]
                 [ 9; 8; 7; 6; 5 ]
                 139629729 |]
        yield [| [| 3
                    52
                    1001
                    52
                    -5
                    52
                    3
                    53
                    1
                    52
                    56
                    54
                    1007
                    54
                    5
                    55
                    1005
                    55
                    26
                    1001
                    54
                    -5
                    54
                    1105
                    1
                    12
                    1
                    53
                    54
                    53
                    1008
                    54
                    0
                    55
                    1001
                    55
                    1
                    55
                    2
                    53
                    55
                    53
                    4
                    53
                    1001
                    56
                    -1
                    56
                    1005
                    56
                    6
                    99
                    0
                    0
                    0
                    0
                    10 |]
                 [ 9; 7; 8; 5; 6 ]
                 18216 |]
    }

[<Fact>]
let ``Test solve 2`` () =
    let actual =
        "../../../Data/07.txt"
        |> parseFirstLine (splitBy "," asIntArray)
        |> solve2
    Assert.Equal(19384820L, actual)


[<Theory>]
[<MemberData("testCases2")>]
let ``solve2 test`` program x expected =
    let actual = initializeAmps program x
    Assert.Equal(expected, actual)

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
let ``Solve 1``() =
    let actual =
        "../../../Data/07.txt"
        |> parseFirstLine (splitBy "," asIntArray)
        |> solve
    Assert.Equal(17790L, actual)

[<Theory>]
[<MemberData("testCases")>]
let ``test run seq`` program x expected =
    let actual = runSeq program (x |> Array.toSeq)
    Assert.Equal(expected, actual)

[<Theory>]
[<MemberData("testCases")>]
let ``solve test`` program x expected =
    let actual = solve program
    Assert.Equal(expected, actual)
