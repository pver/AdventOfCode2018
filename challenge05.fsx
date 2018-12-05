let parseItems (inputPath:string) = 
    inputPath 
    |> System.IO.File.ReadAllText
    |> fun x -> x.ToCharArray()
    |> Array.toList

let inputChallenge5 = parseItems "input05.txt"

let haveOppositePolarity (c1:char) (c2:char) = abs((int c1)-(int c2)) = 32

let rec challenge5A (input:char list) =
    let result = 
        input 
        |> List.fold 
            (
                fun (consider,newlist) c -> 
                    match consider with
                    | [] -> ([c], newlist)
                    | [x] -> if haveOppositePolarity x c then ([], newlist) else ([c], newlist @ consider)
            ) ([],[])
        |> fun (c,newlist) -> newlist @ c

    if input.Length = result.Length 
    then result 
    else challenge5A result

let challenge5B (input:char list) =
    let lengths=
        [|
        (input |> List.filter (fun x -> x <> 'a' && x <> 'A')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'b' && x <> 'B')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'c' && x <> 'C')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'd' && x <> 'D')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'e' && x <> 'E')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'f' && x <> 'F')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'g' && x <> 'G')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'h' && x <> 'H')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'i' && x <> 'I')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'j' && x <> 'J')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'k' && x <> 'K')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'l' && x <> 'L')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'm' && x <> 'M')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'n' && x <> 'N')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'o' && x <> 'O')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'p' && x <> 'P')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'q' && x <> 'Q')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'r' && x <> 'R')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 's' && x <> 'S')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 't' && x <> 'T')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'u' && x <> 'U')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'v' && x <> 'V')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'w' && x <> 'W')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'x' && x <> 'X')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'y' && x <> 'Y')) |> challenge5A |> List.length
        (input |> List.filter (fun x -> x <> 'z' && x <> 'Z')) |> challenge5A |> List.length
    |] 
    
    lengths
    |> Array.min

printfn "Solution 5A Example 1: %d" (List.length <| challenge5A ("dabAcCaCBAcCcaDA".ToCharArray() |> Array.toList))
printfn "Solution 5A: %d" (List.length <| challenge5A inputChallenge5)

printfn "Solution 5B Example 1: %d" (challenge5B ("dabAcCaCBAcCcaDA".ToCharArray() |> Array.toList))
printfn "Solution 5B: %d" (challenge5B inputChallenge5)