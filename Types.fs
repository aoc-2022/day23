module day23.Types

type Pos = int*int
let private surrounding ((x,y):Pos) =
    [(x-1,y-1);(x,y-1);(x+1,y-1);
     (x-1,y);           (x+1,y);
     (x-1,y+1);(x,y+1);(x+1,y+1)]
type Elves(elves:Set<Pos>) =
    member this.Set = elves
    member this.Alone =
        let alone (elf:Pos) =
            let neighbors = surrounding elf
                            |> List.filter elves.Contains
            neighbors.IsEmpty
        elves |> Set.filter alone
        
    member this.Social =
        let social (elf:Pos) =
            let neighbors = surrounding elf
                            |> List.filter elves.Contains
            neighbors.Length > 0
        elves |> Set.filter social

    override this.ToString() = $"{elves}"
    
let printElves (elves:Elves) =
    let minX = elves.Set |> Set.map fst |> Set.minElement
    let maxX = elves.Set |> Set.map fst |> Set.maxElement
    let minY = elves.Set |> Set.map snd |> Set.minElement
    let maxY = elves.Set |> Set.map snd |> Set.maxElement
    let ys = seq {minY .. maxY} |> Seq.toList
    let xs = seq {minX .. maxX} |> Seq.toList
    let printElf (pos:Pos) = if elves.Set.Contains pos then printf "#" else printf "."
    let rec printLine (y:int) (xs:int List) =
        match xs with
        | [] -> printfn ""
        | x::rest ->
            printElf (x,y) 
            printLine y rest
    ys |> List.map (fun y -> printLine y xs)

let emptyGround (elves:Elves) =
    let minX = elves.Set |> Set.map fst |> Set.minElement
    let maxX = elves.Set |> Set.map fst |> Set.maxElement
    let minY = elves.Set |> Set.map snd |> Set.minElement
    let maxY = elves.Set |> Set.map snd |> Set.maxElement
    let squareSize = (maxX-minX+1) * (maxY-minY+1)
    squareSize - elves.Set.Count
    