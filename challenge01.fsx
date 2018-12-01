let parseItems (inputPath:string) =
    inputPath 
    |> System.IO.File.ReadAllLines
    |> Array.map (fun i -> int i)

let inputChallenge1 = parseItems "input01.txt"

let challenge1a (inputs:int[]) =
    let numbers = inputs |> Array.fold (fun acc elem -> acc + elem) 0
    numbers

let rec challenge1b_process_inputs (curr_freq:int) (inputs:int[]) (frequencies_found:System.Collections.Generic.HashSet<int>) =
    let rec loop freq i = 
        
        match (i<inputs.Length) with
        | false -> challenge1b_process_inputs freq inputs frequencies_found
        | true -> 
            let new_freq = freq + inputs.[i]
            match (frequencies_found.Add new_freq) with 
            | false -> new_freq 
            | true -> loop new_freq (i+1)
    
    loop curr_freq 0

let challenge1b (inputs:int[]) =
    let founds = System.Collections.Generic.HashSet<int>()
    founds.Add 0 |> ignore
    challenge1b_process_inputs 0 inputs founds
        
printfn "Solution 1A Example 1: %d <= sould be 3" <| challenge1a [|+1; +1; +1|]
printfn "Solution 1A Example 2: %d <= sould be 0" <| challenge1a [|+1; +1; -2|]
printfn "Solution 1A Example 3: %d <= sould be -6" <| challenge1a [|-1; -2; -3|]
printfn "Solution 1A: %d" <| challenge1a inputChallenge1

printfn "Solution 1B Example 1: %d <= sould be 0" <| challenge1b [|+1; -1|] 
printfn "Solution 1B Example 2: %d <= sould be 10" <| challenge1b [|+3; +3; +4; -2; -4|] 
printfn "Solution 1B Example 3: %d <= sould be 5" <| challenge1b [|-6; +3; +8; +5; -6|] 
printfn "Solution 1B Example 4: %d <= sould be 14" <| challenge1b [|+7; +7; -2; -7; -4|]
printfn "Solution 1B: %d" <| challenge1b inputChallenge1 