module day23.Task1MoveDirs

open day23.Types 
let north ((x, y): Pos) : Pos * (Pos list) =
    (x, y - 1), [ (x - 1, y - 1); (x, y - 1); (x + 1, y - 1) ]

let south ((x, y): Pos) : Pos * (Pos list) =
    (x, y + 1), [ (x - 1, y + 1); (x, y + 1); (x + 1, y + 1) ]
 
let west ((x,y):Pos) : Pos * (Pos list) =
    (x-1,y), [x-1,y-1; x-1,y; x-1,y+1]
    
let east ((x,y):Pos) : Pos * (Pos list) =
    (x+1,y), [x+1,y-1; x+1,y; x+1,y+1]

type Direction(dirs:(Pos -> Pos * (Pos list)) list) =
    member this.Dirs = dirs
   
    member this.Rotate () =
        let [a;b;c;d] = dirs
        let dirs = [b;c;d;a]
        Direction(dirs)
        
    static member task1Init = [north;south;west;east] |> Direction 
    