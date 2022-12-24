open day23.Task1MoveDirs
open day23.Types
open day23
let elves = Input.readElves "/tmp/aoc/input"
let direction = Direction.task1Init

printfn $"{elves}"

let whereToMoveTo (elf:Pos) (elves:Elves) (direction:Direction) : Pos =
    let proposedMoves = direction.Dirs |> List.map (fun d -> d elf)
    let validMove ((_,dests):Pos*(Pos list)) : bool =
        dests |> List.filter elves.Set.Contains |> List.isEmpty
    // printfn $"proposedMoves({elf} = {proposedMoves}"
    let proposedMoves = proposedMoves |> List.filter validMove
    // printfn $"valid: {proposedMoves}"
    let chosen = if proposedMoves.IsEmpty then elf else fst proposedMoves.Head
    // printfn $"chosen: {chosen}"
    chosen 

let move (elves:Elves,direction:Direction) =
    // first half 
    let movers = elves.Social
    let movers = movers |> Set.toList
                 |> List.map (fun elf -> elf,(whereToMoveTo elf elves direction))
                 |> List.filter (fun (elf,dest) -> elf <> dest)
    // second half
    let validDests : Set<Pos> = movers
                                |> List.map snd
                                |> List.groupBy id
                                |> List.filter (fun (i,l) -> l.Length = 1)
                                |> List.map fst 
                                |> Set.ofList
    let movers = movers |> List.filter (fun (elf,dest) -> validDests.Contains dest)
    let old = movers |> List.map fst |> Set.ofList
    let movedInto = movers |> List.map snd |> Set.ofList
    Elves(Set.union (Set.difference elves.Set old) movedInto)
    
let rec moveTimes (n:int) (elves:Elves) (direction:Direction) =
    if n = 0 then elves
    else
        let elves = move (elves,direction)
        let direction = direction.Rotate ()
        moveTimes (n-1) elves direction
        
let elves10 = moveTimes 10 elves direction
printfn "###10"
printElves elves10

let empty10 = emptyGround elves10

printfn $"### empty 10 = {empty10}"

let rec findZeroMove (elves:Elves) (direction:Direction) =
        let newElves = move (elves,direction)
        let direction = direction.Rotate ()
        if newElves.Set = elves.Set then
            1
        else 1 + (findZeroMove newElves direction) 


let standStill = findZeroMove elves direction

printfn $"Standstill: {standStill}"
