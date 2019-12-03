module _02

open Common

let parse = parseFirstLine (splitBy "," asIntArray) "../../../Data/02.txt"

let replace (x,y) (xs:int[]) =
    xs.[1] <- x
    xs.[2] <- y
    xs

let apply (xs: int[]) read1 read2 write f =
    xs.[write] <- (f xs.[read1] xs.[read2])
    xs

let getFunc i xs =
    let sub = Array.sub xs i 4
    apply xs sub.[1] sub.[2] sub.[3]

let rec readNextInstruction i (xs: int[]) =
    match xs.[i] with
        | 1 -> 
            getFunc i xs (+)
            |> readNextInstruction (i+4)
        | 2 -> 
            getFunc i xs (*)
            |> readNextInstruction (i+4)
        | 99 -> xs        
        | x -> x |> sprintf "Unknown OpCode %i" |> failwith


let solve x =
    parse
    |> Array.copy
    |> replace x
    |> readNextInstruction 0
    |> Array.item 0

let solve1 = solve (12, 2)

let solve2 solution =
    let rec getSolution x xs =
        xs
        |> List.item x
        |> solve 
        |> function
            | y when y = solution -> List.item x xs
            | _ -> getSolution (x+1) xs
    
    List.allPairs [0..99] [0..99]
    |> getSolution 0