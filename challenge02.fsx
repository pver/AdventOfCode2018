let parseItems (inputPath:string) = 
    inputPath 
    |> System.IO.File.ReadAllLines

let inputChallenge2 = parseItems "input02.txt"

let charCounter (s:string) =
    s.ToCharArray()
    |> Array.groupBy (fun x->x)
    |> Array.map (fun (c,instances) -> (c, Array.length instances))

let challenge2a (inputs:string[]) =
    let countsByChar = inputs |> Array.map charCounter
    let twos = countsByChar |> Array.filter (fun x-> Array.exists (fun (_, count)-> count = 2) x) |> Array.length
    let threes = countsByChar |> Array.filter (fun x-> Array.exists (fun (_, count)->count = 3) x) |> Array.length

    twos * threes


let inputChallenge2AExample = [|
    "abcdef" 
    "bababc" 
    "abbcde" 
    "abcccd" 
    "aabcdd" 
    "abcdee" 
    "ababab" 
|]
printfn "Solution 2A Example 1: %d <= sould be 12" <| challenge2a inputChallenge2AExample
printfn "Solution 2A: %d" <| challenge2a inputChallenge2

// ----------------- Part B

let differOnePos (first:char[]) (second:char[]) = 
    Array.zip first second 
    |> Array.filter (fun (x,y) -> x <> y)
    |> Array.length
    |> (fun count -> count = 1)

let removeDiffer (first:char[]) (second:char[]) = 
    Array.zip first second 
    |> Array.filter (fun (x,y) -> x = y)
    |> Array.map (fun (x,_) -> x)

let inputChallenge2ExampleB = [|
    "abcde"
    "fghij"
    "klmno"
    "pqrst"
    "fguij"
    "axcye"
    "wvxyz"
|]


let challenge2b (inputs:string[]) =
    let inputsAsChars = inputs |> Array.map (fun x -> x.ToCharArray())
    let inputCount = Array.length inputs

    let (matchA, matchB) = 
        seq { for i in 0 .. (inputCount-1) do
                  for j in i .. (inputCount-1) do
                    yield (inputsAsChars.[i], inputsAsChars.[j])
        }
        |> Seq.filter (fun (first, second) -> differOnePos first second)
        |> Seq.head
    
    System.String(removeDiffer matchA matchB)

printfn "Solution 2B Example 1: %s" <| challenge2b inputChallenge2ExampleB
printfn "Solution 2B: %s" <| challenge2b inputChallenge2