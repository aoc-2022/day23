module day23.Input

open day23.Types

open System.IO

let private parseLine (y:int,line:string) : Pos list = 
    line.ToCharArray ()
        |> Array.toList
        |> List.indexed 
        |> List.filter (fun (_,c) -> c = '#')
        |> List.map (fun (x,_) -> (x,y))
         
               
let readElves (fileName: string) =
        File.ReadAllLines fileName 
               |> Array.toList
               |> List.indexed
               |> List.map parseLine
               |> List.concat
               |> Set.ofList
               |> Elves
            
            