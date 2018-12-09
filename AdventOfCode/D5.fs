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

let seqStringToCharList strings =
    strings |> Seq.head |> Seq.toList

let day5test1 polymerStrings =
    polymerStrings
    |> seqStringToCharList
    |> processAndVerify 0 
    |> Seq.length

let processWithoutUnit polymerString unitChar =
    polymerString
    |> List.filter (fun polymerChar -> polymerChar <> unitChar && polymerChar <> System.Char.ToUpper(unitChar))
    |> processAndVerify 0
    |> Seq.length

let day5test2 polymerStrings =
    seq { for x in [97..122] do yield char x } 
    |> Seq.map (fun unitChar -> (processWithoutUnit (polymerStrings |> seqStringToCharList) unitChar, unitChar))
    |> Seq.sortBy (fun unitCharCount -> fst unitCharCount)
    |> Seq.head