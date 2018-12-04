module D3

type Claim = {
    id: int;
    startLeft: int;
    startTop: int;
    width: int;
    height: int
}

let getClaimFromString (claimString:string) = 
    {
        id = (int claimString.[1..claimString.IndexOf(" ")-1]);
        startLeft = (int claimString.[claimString.IndexOf("@")+1..claimString.IndexOf(",")-1]);
        startTop = (int claimString.[claimString.IndexOf(",")+1..claimString.IndexOf(":")-1]);
        width = (int claimString.[claimString.IndexOf(":")+1..claimString.IndexOf("x")-1]);
        height = (int claimString.[claimString.IndexOf("x")+1..claimString.Length-1])
    }

let getCoordinatesFromClaim claim = 
    seq { for i in claim.startLeft .. (claim.startLeft + claim.width - 1) do 
            for j in claim.startTop .. (claim.startTop + claim.height - 1) do 
                yield i, j}

let day3test1 claims = 
    claims
    |> Seq.map (getClaimFromString)
    |> Seq.map (getCoordinatesFromClaim)
    |> Seq.concat
    |> Seq.groupBy (fun x -> x)
    |> Seq.map (fun result -> (fst result), (snd result |> Seq.length))
    |> Seq.filter (fun x -> snd x > 1)
    |> Seq.length
