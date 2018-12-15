module D6

type Boundary = {
    MinX: int;
    MaxX: int;
    MinY: int;
    MaxY: int;
}

type Coordinate = int*int

type DistanceMatch = 
    | Multiple
    | SingleCoordinate of Coordinate

type ManhattanDistance =  {
    BaseCoordinate: Coordinate;
    Match: DistanceMatch;
    Distance: int;
}

let parseCoordinates (coordinateString:string) = 
    int coordinateString.[0..coordinateString.IndexOf(",")-1],
    int coordinateString.[coordinateString.IndexOf(" ")+1..coordinateString.Length-1]

let returnLower int1 int2 =
    if int1 < int2 then int1 else int2

let returnHigher int1 int2 =
    if int1 > int2 then int1 else int2

let calculateManhattanDistance (coordinate1:Coordinate) (coordinate2:Coordinate) =
    System.Math.Abs((fst coordinate1) - (fst coordinate2)) + System.Math.Abs((snd coordinate1) - (snd coordinate2))

let rec findBoundary xMin xMax yMin yMax remainingCoordinates =
    match remainingCoordinates with
    | h::t -> 
        findBoundary (returnLower (fst h) xMin) (returnHigher (fst h) xMax) (returnLower (snd h) yMin) (returnHigher (snd h) yMax) t
    | [] -> { MinX = xMin; MaxX = xMax; MinY = yMin; MaxY = yMax }

let rec determineClosestTarget targets distmatch dist coordinate =
    match targets with
    | h::t -> 
        let md = (calculateManhattanDistance coordinate h)
        match dist with
        | -1 -> 
            determineClosestTarget t (SingleCoordinate h) md coordinate
        | x when x > md -> 
            determineClosestTarget t (SingleCoordinate h) md coordinate
        | y when y = md -> 
            determineClosestTarget t Multiple md coordinate
        | _ -> 
            determineClosestTarget t distmatch dist coordinate
    | [] -> { BaseCoordinate = coordinate; Match = distmatch; Distance = dist }

let keepCoordinate manhattanDistances boundary =
    let boundaryAvoidanceChecks = 
        manhattanDistances
        |> Seq.map (fun md ->
            match md.BaseCoordinate with
            | (x,y) -> 
                (x > boundary.MinX)
                && (x < boundary.MaxX)
                && (y > boundary.MinY)
                && (y < boundary.MaxY)
            | _ -> false)

    Seq.forall (fun check -> check = true) boundaryAvoidanceChecks

let day6test1 coordinatesInput =
    let coordinates = 
        coordinatesInput
        |> Seq.map parseCoordinates
        |> Seq.toList
    
    let boundary = findBoundary 0 0 0 0 coordinates

    let space = seq {
        for x in [boundary.MinX..boundary.MaxX] do
            for y in [boundary.MinY..boundary.MaxY] do
                yield x, y
    }

    space 
    |> Seq.map (determineClosestTarget coordinates Multiple -1)
    |> Seq.filter (fun md -> md.Match <> Multiple)
    |> Seq.groupBy (fun md -> md.Match)
    |> Seq.filter (fun mdGroup -> keepCoordinate (snd mdGroup) boundary)
    |> Seq.map (fun mdGroup -> (snd mdGroup), (snd mdGroup |> Seq.length))
    |> Seq.sortByDescending (fun mdCount -> snd mdCount)