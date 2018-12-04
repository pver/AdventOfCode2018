type textitem = { time:System.DateTime; text:string}
type dayitem = {date:System.DateTime; id:int; awake:int list; asleep:int list}

let parseItem (s:string) : textitem =
    let [|brackedLeft;text|] = s.Split([|']'|])
    let time = System.DateTime.Parse(brackedLeft.Trim().Trim('['))
    { time=time; text=text.Trim()}

let fixItem (item:textitem) = 
    match item.time.Hour with 
    | 23 -> {item with time=(item.time.Date.AddDays(1.0))} 
    | _ -> item

let isGuardIdItem (i:textitem) = i.text.Contains("Guard #")
let isAsleepItem (i:textitem) = i.text.Contains("asleep") 

let processDay ((day,items):System.DateTime*textitem[]) = 
    let id =
        items 
        |> Array.filter isGuardIdItem
        |> Array.head
        |> fun x -> x.text.Replace("Guard #","").Replace(" begins shift","")
        |> int

    let startday = {id=id;date=day;awake=[];asleep=[]}
    
    let (_, day) =
        items 
        |> Array.filter (fun x -> isGuardIdItem(x) = false) 
        |> Array.fold (fun (startminute,intermediateday) x -> 
                                                    let newMinute = x.time.Minute
                                                    let newDayItem = 
                                                        if isAsleepItem x 
                                                        then {intermediateday with awake=intermediateday.awake@[startminute..(newMinute-1)]}
                                                        else {intermediateday with asleep=intermediateday.asleep@[startminute..(newMinute-1)]}
                                                    (newMinute, newDayItem)
                                                    ) (0,startday)
    items 
    |> Array.last 
    |> (fun x ->    if isAsleepItem x
                    then {day with asleep=day.asleep@[x.time.Minute..59]}
                    else {day with awake=day.awake@[x.time.Minute..59]})
    

let parseItems (inputPath:string) = 
    inputPath 
    |> System.IO.File.ReadAllLines
    |> Array.map parseItem
    |> Array.map fixItem
    |> Array.sortBy (fun x -> x.time)
    |> Array.groupBy (fun x-> x.time.Date)
    |> Array.map processDay
    

let inputChallenge4 = parseItems "input04.txt"

let sleepTimesByAgentId (items:dayitem[]) =
        items 
        |> Array.groupBy (fun x -> x.id) 
        |> Array.map (fun (id, iditems) -> 
                        let alltimes = (iditems |> Array.collect (fun z -> z.asleep |> List.toArray))
                        (id, alltimes))

let challenge4A (items:dayitem[]) =
    let agentSleepTimes = sleepTimesByAgentId items

    let (agentId, sleepTimes) = 
        agentSleepTimes
        |> Array.sortByDescending (fun (_, sleeptimes) -> sleeptimes |> Array.length)
        |> Array.head

    let minute = 
        sleepTimes 
            |> Array.groupBy id 
            |> Array.sortByDescending (fun (_,times) -> times |>Array.length)
            |> Array.head
            |> (fun (time,_) -> time)
    agentId * minute
    
let challenge4B (items:dayitem[]) =
    let agentSleepTimes = sleepTimesByAgentId items
    
    let (agentId, agentSleepTimes) = 
        agentSleepTimes
        |> Array.sortByDescending (fun (_, sleeptimes) -> sleeptimes |> Array.groupBy id |> Array.sortByDescending (fun (_,x) -> x |> Array.length) |> Array.map (fun (_,x) -> x |> Array.length))
        |> Array.head
    
    agentId * (agentSleepTimes |> Array.groupBy id |> Array.sortByDescending (fun (_,x) -> x |> Array.length) |> Array.map (fun (x,_) -> x) |> Array.head)

printfn "Solution 4A: %A" <| challenge4A inputChallenge4
printfn "Solution 4B: %A" <| challenge4B inputChallenge4