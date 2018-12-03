module D2

let countInstances (id:string) =
    Seq.toList id
    |> Seq.groupBy (fun x -> x)
    |> Seq.map (fun result -> (fst result), (snd result |> Seq.length))
    |> Seq.filter (fun result -> snd result = 2 || snd result = 3)

let updateCountFor count values test =
    if Seq.exists (fun x -> snd x = test) values then
        count + 1
    else 
        count

let day2test1 ids = 
    ids
    |> Seq.map countInstances
    |> Seq.fold (fun state item -> (updateCountFor (fst state) item 2), (updateCountFor (snd state) item 3)) (0,0)
    |> (fun result -> fst result * snd result)

let goodMatch (id1:string) (id2:string) =
    (Seq.toList id1, Seq.toList id2)
    ||> List.fold2 (fun state x1 x2 -> if x1 = x2 then state + 1 else state) 0
    |> (fun x -> x = (id1.Length - 1))

let buildMatchingId (id1:string) (id2:string) =
    printfn "id1 %A id2 %A" id1 id2
    (Seq.toList id1, Seq.toList id2)
    ||> List.fold2 (fun state x1 x2 -> if x1 = x2 then x1::state else state) []
    |> List.rev
    |> sprintf "%A"

let testForMatches allIds id =
    allIds
    |> Seq.exists (fun x -> goodMatch id x)
    |> (fun result -> if result then id else "")

let rec buildMatches allIds matches comparisonIds =
    match comparisonIds with
    | h::t -> buildMatches allIds ((testForMatches allIds h)::matches) t
    | [] -> matches 

let day2test2 ids =
    Seq.toList ids
    |> buildMatches ids List.Empty 
    |> List.filter (fun x -> x <> "")
    |> List.reduce (buildMatchingId)
