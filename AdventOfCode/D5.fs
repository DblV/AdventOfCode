module D5

let unitsReact unit1 unit2 =
    (unit1 <> unit2) && (System.Char.ToUpper(unit1) = System.Char.ToUpper(unit2))

let rec processPolymer polymer currentUnit processStr = 
    match processStr with
    | h::t -> 
        if (unitsReact currentUnit h) then
            processPolymer (polymer |> List.tail) System.Char.MinValue t
        else
            processPolymer (h::polymer) h t
    | [] -> (polymer |> List.rev)

let rec processAndVerify previousLength data = 
    if (data |> List.length) = previousLength then
        data
    else
        processAndVerify (data |> List.length) (processPolymer List.empty<char> System.Char.MinValue data)

let day5test1 polymerStrings =
    polymerStrings
    |> Seq.head
    |> Seq.toList
    |> processAndVerify 0 
    |> Seq.length
