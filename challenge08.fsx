let parseItem (s:string) = s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int

let parseItems (inputPath:string) = 
    inputPath 
    |> System.IO.File.ReadAllText
    |> parseItem    

let inputExample = parseItem "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

let inputChallenge8 = parseItems "input08.txt"

type node = {childCount:int; metadataCount:int;childNodes:node list;metadata:int list}

let challenge8 (input:int[]) =
    
    let rec readNode pos =
        
        let childCount = input.[pos]
        let metadataCount = input.[pos+1]

        let (childNodes, endpos ) = 
            [0 .. (childCount-1)] 
            |> List.fold (fun (nodes, new_pos) _ -> 
                    let (node, next_end_pos) = readNode new_pos
                    ((nodes @ [node]), next_end_pos)
                ) ([], (pos+2))
        let metadata = Array.sub input endpos metadataCount |> Array.toList

        let nodeEndPos = (endpos+metadataCount)
        ({childCount=childCount;metadataCount=metadataCount;childNodes=childNodes;metadata=metadata}, nodeEndPos)
    
    readNode 0

let challenge8A (input:int[]) =
    let (nodes,_) = challenge8 input
    let rec calcMetaData node =
        node.metadata 
        |> List.sum
        |> (+) (node.childNodes |> List.map calcMetaData |> List.sum)
    
    calcMetaData nodes

let challenge8B (input:int[]) =
    let (nodes,_) = challenge8 input


    let rec calcMetaData node =
        match node.childCount with
        | 0 -> node.metadata |> List.sum
        | _ -> node.metadata 
               |> List.map (
                    fun index -> 
                        match index with
                        | 0 -> 0
                        | _ -> if (index-1<node.childCount) then calcMetaData node.childNodes.[index-1] else 0
                  )
               |> List.sum
    
    calcMetaData nodes

//printfn "%A" inputExample

printfn "Challenge A example : %A " <| challenge8A inputExample
printfn "Challenge A : %A" <| challenge8A inputChallenge8

printfn "Challenge B example : %A " <| challenge8B inputExample
printfn "Challenge B : %A" <| challenge8B inputChallenge8