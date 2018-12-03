module D1

let alterFrequency frequency (alteration:string) =
    match alteration.[0] with 
    | '+' -> frequency + (alteration.[1..alteration.Length-1] |> int)
    | '-' -> frequency - (alteration.[1..alteration.Length-1] |> int)
    | _ -> frequency

let rec findFirstDuplicate allAlterations remainingAlterations currentFrequency previousFrequencies =
    if (List.contains currentFrequency previousFrequencies) then
        currentFrequency
    else
        match remainingAlterations with
        | alteration::remainingAlterationsTail -> 
            findFirstDuplicate allAlterations remainingAlterationsTail (alterFrequency currentFrequency alteration) (currentFrequency::previousFrequencies)
        | [] -> 
            findFirstDuplicate allAlterations allAlterations currentFrequency previousFrequencies

let day1test1 alterations = 
    Seq.fold (fun result alteration -> alterFrequency result alteration) 0
        
let day1test2 alterations = 
    let masterlist = alterations |> Seq.toList
    findFirstDuplicate masterlist masterlist 0 []
