open _02
module Program = let [<EntryPoint>] main _ = 
    solve2 19690720 
    |> printf "%A"
    
    0