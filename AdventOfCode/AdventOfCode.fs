module AdventOfCode
open Lib
open D1
open D2

[<EntryPoint>]
let main argv =
    match argv.[0] with
    | "1.1" -> printfn "%A" (day1test1 (readLines argv.[1]))
    | "1.2" -> printfn "%A" (day1test2 (readLines argv.[1]))
    | "2.1" -> printfn "%A" (day2test1 (readLines argv.[1]))
    | "2.2" -> printfn "%A" (day2test2 (readLines argv.[1]))
    | _ -> printfn "Invalid argument"
    0 // return an integer exit code
