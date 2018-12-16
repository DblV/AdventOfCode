module D7

type InstructionOrder = {
    Prerequisite: char;
    NextSteps: List<char>;
}

let parseInstruction (instruction:string) =
    instruction.[5], instruction.[36]

let foldNextSteps (instructionList:seq<char*char>) =
    instructionList
    |> Seq.fold (fun state nextStep -> (snd nextStep)::state) List.Empty

let findStartingInstructions instructions =
    seq { for x in [65..90] do yield char x }
    |> Seq.filter (fun possibleFirstLetter ->
        not (Seq.exists (fun x -> x = possibleFirstLetter) instructions))

let tryToMatch tup char2 =
    match (fst tup) with
    | c when c = char2 -> Some c
    | _ -> None

let getNextInstructions nextInstructions unlockedInstructions instructionOrders =
    let remainingLockedNextSteps = 
        instructionOrders
        |> Seq.map (fun i -> i.NextSteps)
        |> Seq.fold (fun state i -> List.append state i) List.empty

    unlockedInstructions
    |> List.filter (fun u -> not (Seq.exists (fun i -> i = u) remainingLockedNextSteps))
    |> List.append nextInstructions
    |> List.groupBy (fun i -> i)
    |> List.map (fun (i, gr) -> i)
    |> List.sort

let rec createInstructions (nextInstructions:list<char>) (instructionOrders:seq<InstructionOrder>) (instructions:list<char>) =
    match nextInstructions with
    | h::t -> 
        let insOrd = Seq.tryFind (fun x -> x.Prerequisite = h) instructionOrders
        let remainingInstructionOrders = Seq.filter (fun s -> s.Prerequisite <> h) instructionOrders
        match (insOrd) with 
        | Some i ->
            createInstructions (getNextInstructions t i.NextSteps remainingInstructionOrders) remainingInstructionOrders (h::instructions)
        | None -> 
            createInstructions t remainingInstructionOrders (h::instructions)
    | [] -> 
        instructions

let day7test1 instructions = 
    let allInstructions =
        instructions
        |> Seq.map parseInstruction
        |> Seq.groupBy (fun instr -> (fst instr))
        |> Seq.map (fun instrGroup -> 
            { Prerequisite = (fst instrGroup); NextSteps = (foldNextSteps (snd instrGroup)); })

    let allNextSteps = Seq.fold (fun state nextSteps -> List.append state nextSteps) List.empty (Seq.map (fun i -> i.NextSteps) allInstructions)

    createInstructions ((findStartingInstructions allNextSteps) |> Seq.toList |> List.sort) allInstructions List.empty
    |> List.rev
