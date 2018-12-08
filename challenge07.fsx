let parseItem (s:string) = (s.[5], s.[36]) //Example input= "Step O must be finished before step C can begin."

let parseItems (inputPath:string) = 
    inputPath 
    |> System.IO.File.ReadAllLines
    |> Array.sort
    |> Array.map parseItem    

let inputExample = 
    [|  "Step C must be finished before step A can begin."
        "Step C must be finished before step F can begin."
        "Step A must be finished before step B can begin."
        "Step A must be finished before step D can begin."
        "Step B must be finished before step E can begin."
        "Step D must be finished before step E can begin."
        "Step F must be finished before step E can begin."
    |] |> Array.sort |> Array.map parseItem

let inputChallenge7 = parseItems "input07.txt"

let getDependencies (input:(char*char)[]) =
    let deps = new System.Collections.Generic.Dictionary<char, char[]>()

    input 
    |> Array.groupBy (fun (a,_) -> a) 
    |> Array.map (fun (x,y) -> (x, y |> Array.collect(fun (_,b)->[|b|])))
    |> Array.iter (fun (x,y) -> deps.[x]<-y) 
    deps

let challengeB (input:(char*char)[]) (workerCount:int) (baseTime:int) =
    let dep = getDependencies input

    let froms() = System.Linq.Enumerable.ToArray(dep.Keys)
    let tos() = System.Linq.Enumerable.ToArray(dep.Values) |> Array.collect id |> Array.distinct |> Array.sort

    let roots = froms() |> Array.except (tos()) |> Array.sort |> Array.toList

    let rec solve timer dones (waiting:(char*int) list) todos =
        //printfn "[%i] dones: '%A' waiting: '%A' todos: '%A' " timer dones waiting todos
        match (waiting, todos) with 
        | ([],[]) -> (timer, dones)
        | _ -> 


            let update_waiting = waiting |> List.map (fun (x,y) -> (x, y-1))

            let completed = update_waiting |> List.filter (fun (_,y) -> y=0)|> List.map (fun (x,_) -> x)
            let new_dones = dones @ completed

            let waiting_without_completed = update_waiting |> List.filter (fun (x,_) -> not (completed |> List.contains x))

            let new_todos = 
                todos @
                    match dep.Count with
                    | 0 -> []
                    | _ -> 
                            completed 
                            |> List.collect (fun (x)->
                                let nexts = (dep.[x]|>Array.toList)
                                dep.Remove(x) |> ignore
                                nexts
                                )
                |> List.distinct |> List.sort

            let new_tos = tos()
            let next = 
                new_todos 
                |> List.toSeq 
                |> Seq.filter (fun x->not (new_tos |> Array.contains x)) // don't take the ones that aren't done yet
                |> Seq.sort 
                |> Seq.truncate (workerCount-waiting_without_completed.Length) // don't take more than we have workers available
                |> Seq.map (fun x -> (x, (int x)-64 + baseTime)) 
                |> Seq.toList
            
            let new_waiting = waiting_without_completed @ next      
            
            solve (timer + 1) new_dones new_waiting (new_todos |> List.except (next|>List.map (fun (x,_)->x)))
    
    solve -1 [] [] roots

let challengeA input =
    let (_, result) = challengeB input 1 0
    result

printfn "Example output: %A" (new System.String(challengeA inputExample|> List.toArray))
printfn "Challenge output: %A" (new System.String(challengeA inputChallenge7|> List.toArray))

let (exampleTime, exampleResult) = challengeB inputExample 2 0
printfn "Example output: %A, time: %A" (new System.String(exampleResult|> List.toArray)) exampleTime
let (challengeTime, challengeResult) = challengeB inputChallenge7 5 60
printfn "Challenge B output: %A, time: %A" (new System.String(challengeResult|> List.toArray)) challengeTime