// #1 @ 1,3: 4x4
type item = { id:int; startx:int; starty:int; width:int; height:int}

let parseItem (s:string) : item =
    let [|atLeft;atRight|] = s.Split([|'@'|])
    let id = atLeft.Trim().Trim('#')
    let [|colonLeft;colonRight|] = atRight.Trim().Split([|':'|])
    let [|startx;starty|] = colonLeft.Trim().Split([|','|])
    let [|width;height|] = colonRight.Trim().Split([|'x'|])
    { id=(int id); startx=(int startx); starty=(int starty); width=(int width); height=(int height)}

let parseItems (inputPath:string) = 
    inputPath 
    |> System.IO.File.ReadAllLines
    |> Array.map parseItem

let inputChallenge3 = parseItems "input03.txt"

let putItemOnBoard (board:(int*item list)[][]) (item:item)=
    [ for i in item.startx .. (item.startx + item.width - 1) do
                  for j in item.starty .. (item.starty + item.height - 1) do
                    yield (i, j)
        ] |> List.iter (fun (y,x) -> 
            let (count, items) = board.[x].[y] 
            board.[x].[y] <- (count + 1, ([item] @ items)))

let createBoard (items:item[]) =
    let emptyItems : item list = []
    let board = [| for i in 0 .. 999 do
                    yield Array.create 1000 (0,emptyItems) |]
    items |> Array.iter (putItemOnBoard board)
    board

let hasMultiple ((count,_):int*item list) = count>1

let challenge3a (items:item[]) =
    createBoard items
    |> Array.fold (fun acc x -> acc + (x |> Array.filter hasMultiple |> Array.length)) 0

let challenge3b (items:item[]) =
    let board = createBoard items
    
    let invalidIds = 
        board
        |> Array.collect (fun x -> x |> Array.filter hasMultiple |> Array.map (fun (_,y)->y|>List.map (fun it -> it.id)))
        |> Array.collect List.toArray
    items 
    |> Array.map (fun x -> x.id) 
    |> Array.except invalidIds
    |> Array.exactlyOne

let inputChallenge3AExample = [|
    { id=1; startx=1; starty=3; width=4; height=4}
    { id=2; startx=3; starty=1; width=4; height=4}
    { id=3; startx=5; starty=5; width=2; height=2}
|]

printfn "Solution 3A Example 1: %A" <| challenge3a inputChallenge3AExample
printfn "Solution 3A: %A" <| challenge3a inputChallenge3

printfn "Solution 3B Example 1: %A" <| challenge3b inputChallenge3AExample
printfn "Solution 3B: %A" <| challenge3b inputChallenge3