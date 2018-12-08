module D4

type GuardInfo = 
    | Awake
    | Asleep
    | GuardId of int

type NapData = {
    fullDate: string;
    datePart: string;
    minute: int;
    info: GuardInfo;
}

type NapTime = {
    guardId: int
    minute: int
}

let parseNapData (nap:string) =
    {
        fullDate = nap.[1..nap.IndexOf("]")-1];
        datePart = nap.[1..nap.IndexOf(" ")-1];
        minute = int (nap.[nap.IndexOf(":")+1..nap.IndexOf("]")-1]);
        info = 
            match nap.[nap.IndexOf("]")+2..nap.Length-1] with
            | "falls asleep" -> Asleep
            | "wakes up" -> Awake
            | _ -> GuardId (int (nap.[nap.IndexOf("#")+1..nap.IndexOf("begins")-2]))
    }

let addNapSchedules id startMin endMin =
    if startMin < endMin then
        seq { for x in [startMin..endMin] do yield { guardId = id; minute = x; } }
    else
        Seq.empty<NapTime>

let rec buildNapSchedule guardId prevMinute napSchedule napData = 
    match napData with
    | h::t ->
        match h.info with
        | Awake -> 
            buildNapSchedule guardId h.minute (Seq.append napSchedule (addNapSchedules guardId prevMinute (h.minute-1))) t
        | Asleep -> 
            buildNapSchedule guardId h.minute napSchedule t
        | (GuardId x) -> 
            buildNapSchedule (int x) 0 napSchedule t
    | [] -> napSchedule

let day4test1 naps = 
    let napSchedule = 
        naps
        |> Seq.map parseNapData
        |> Seq.sortBy (fun nap -> nap.fullDate)
        |> Seq.toList
        |> buildNapSchedule 0 0 List.empty<NapTime>

    let sleepyGuardId = 
        napSchedule
        |> Seq.groupBy (fun napTime -> napTime.guardId)
        |> Seq.map (fun napTimeGroup -> (snd napTimeGroup), (snd napTimeGroup |> Seq.length))
        |> Seq.sortByDescending snd
        |> Seq.head |> fst |> Seq.head
        |> (fun napTime -> napTime.guardId)
     
    let sleepiestMinuteForGuard = 
        napSchedule
        |> Seq.filter (fun napTime -> napTime.guardId = sleepyGuardId)
        |> Seq.groupBy (fun napTime -> napTime.guardId, napTime.minute)
        |> Seq.map (fun napTimeGroup -> (snd napTimeGroup), (snd napTimeGroup |> Seq.length))
        |> Seq.sortByDescending snd
        |> Seq.head |> fst |> Seq.head
        |> (fun napTime -> napTime.minute)

    sleepyGuardId * sleepiestMinuteForGuard
 