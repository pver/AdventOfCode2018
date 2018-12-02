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

    let (matchA, matchB) = 
        seq { for row in 0 .. (inputs.Length-1) do
                  for col in row .. (inputs.Length-1) do
                    yield (inputsAsChars.[row], inputsAsChars.[col])
        }
        |> Seq.filter (fun (first, second) -> differOnePos first second)
        |> Seq.head
    
    System.String(removeDiffer matchA matchB)

printfn "Solution 2B Example 1: %s" <| challenge2b inputChallenge2ExampleB
printfn "Solution 2B: %s" <| challenge2b inputChallenge2