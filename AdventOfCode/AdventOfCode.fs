module AdventOfCode
open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader(filePath)
    while not sr.EndOfStream do 
        yield sr.ReadLine ()
}

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

let day1test1 = 
    readLines "C:/Users/wjaco/Documents/GitHub/AdventOfCode/AdventOfCode/values.txt"
    |> Seq.fold (fun result alteration -> alterFrequency result alteration) 0
        
let day1test2 = 
    let masterlist = 
        readLines "C:/Users/wjaco/Documents/GitHub/AdventOfCode/AdventOfCode/values.txt"
        |> Seq.toList
    
    findFirstDuplicate masterlist masterlist 0 []

[<EntryPoint>]
let main argv =
    // printfn "%A" day1test1 
    printfn "%A" day1test2 
    0 // return an integer exit code
