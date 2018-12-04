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
                                                    let addMinutes = [startminute..(newMinute-1)]
                                                    let newDayItem = 
                                                        if isAsleepItem x 
                                                        then {intermediateday with awake=intermediateday.awake@addMinutes} // awake before
                                                        else {intermediateday with asleep=intermediateday.asleep@addMinutes} // asleep before
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

type SleepByAgent = {id:int;sleepTimes:int[];sleepCount:int}

let sleepTimesByAgentId (items:dayitem[]) =
        items 
        |> Array.groupBy (fun x -> x.id) 
        |> Array.map (fun (id, itemsById) -> 
                        let sleepTimes = (itemsById |> Array.collect (fun z -> z.asleep |> List.toArray))
                        {
                            id=id;
                            sleepTimes=sleepTimes;
                            sleepCount=(sleepTimes |> Array.length)
                        }
                     )

let mostOccurred (ints:int[]) = 
    ints 
    |> Array.groupBy id 
    |> Array.sortByDescending (fun (_,t) -> t |> Array.length)
    |> Array.head
    |> (fun (time,_) -> time)

let challenge4A (items:dayitem[]) =
    let agentSleepTimes = sleepTimesByAgentId items

    let agentMostAsleep = 
        agentSleepTimes
        |> Array.sortByDescending (fun x -> x.sleepCount)
        |> Array.head

    let minute = agentMostAsleep.sleepTimes |> mostOccurred
    agentMostAsleep.id * minute
    
let challenge4B (items:dayitem[]) =
    let agentSleepTimes = sleepTimesByAgentId items
    
    let agentMostAsleepOnTheSameMinute = 
        agentSleepTimes
        |> Array.sortByDescending (fun z -> 
                                            z.sleepTimes 
                                            |> Array.groupBy id 
                                            |> Array.sortByDescending (fun (_,x) -> x |> Array.length) 
                                            |> Array.map (fun (_,x) -> x |> Array.length))
        |> Array.head
    
    agentMostAsleepOnTheSameMinute.id * (agentMostAsleepOnTheSameMinute.sleepTimes |> Array.groupBy id |> Array.sortByDescending (fun (_,x) -> x |> Array.length) |> Array.map (fun (x,_) -> x) |> Array.head)

printfn "Solution 4A: %A" <| challenge4A inputChallenge4
printfn "Solution 4B: %A" <| challenge4B inputChallenge4