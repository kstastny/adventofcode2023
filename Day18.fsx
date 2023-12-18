#time

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text.RegularExpressions

let file = File.OpenRead("inputs/input18")
//let file = File.OpenRead("inputs/testData18")



type Direction =
    | Up
    | Down
    | Left
    | Right

type Instructions = { Direction: Direction; Length: int; Color: string }

let parseInstructions (x: string) =
    let parts = x.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    {
        Direction =
            match parts[0] with
            | "L" -> Left
            | "R" -> Right
            | "U" -> Up
            | "D" -> Down
            | _ -> failwith "error"
        Length = parts[1] |> Int32.Parse
        Color = parts[2][2..parts[2].Length - 2] 
    }
    
//Int32.Parse("70c71", System.Globalization.NumberStyles.HexNumber)


    
let parseInstructions2 (x: string) =
    let part1 = parseInstructions x
    {
        Direction =
            match part1.Color[5] with
            | '2' -> Left
            | '0' -> Right
            | '3' -> Up
            | '1' -> Down
            | x ->
                failwithf $"error, unknown color: %A{x} from %A{part1.Color}"
        Length = Int32.Parse(part1.Color[0..4], System.Globalization.NumberStyles.HexNumber)
        Color = part1.Color
    }    

let reader = new StreamReader(file)

let instructions =
    seq {
        while reader.EndOfStream |> not do
            //yield reader.ReadLine() |> parseInstructions2
            yield reader.ReadLine() |> parseInstructions
    }
    |> Array.ofSeq
    
    
let trenchCoordinates =
    instructions
    |> Array.mapFold (fun (x,y) instruction ->
        match instruction.Direction with
        | Right -> [y + 1..y + instruction.Length] |> List.map (fun y2 -> (x, y2))
        | Left -> [y - instruction.Length..y - 1] |> List.rev |> List.map (fun y2 -> (x, y2))
        | Up -> [x - instruction.Length..x - 1] |> List.rev |> List.map (fun x2 -> (x2, y))
        | Down -> [x + 1..x + instruction.Length] |> List.map (fun x2 -> (x2, y))
        |> (fun digged -> digged, digged |> List.last)
        ) (0,0)
    |> fst
    |> List.ofArray
    |> List.concat
//    |> List.distinct
    
    
trenchCoordinates
|> List.pairwise
|> List.where (fun ((x1,y1),(x2,y2)) ->
    Math.Abs(x2 - x1) > 1 || Math.Abs(y2 - y1) > 1
    )
//|> List.where (fun (x,y) -> x = 8)

let intervals (x: int array) =
    //printfn $"intervals %A{x}"
    if x.Length = 0 then
        Array.empty
    else
        seq {
            let mutable iStart = x |> Array.head
            let mutable iEnd = x |> Array.head
            for n in x[1..] do
                if n = iEnd + 1 then
                    iEnd <- n
                else
                    yield iStart, iEnd
                    iStart <- n
                    iEnd <- n
            yield iStart, iEnd
        }
        |> Array.ofSeq
        
   
 /// get the insides of polygon -> returns horizontal lines where the trench is
let breakPoints (trench: (int*int) list) =
     let trenchSet = trench |> Set.ofList
     let minX = trench |> List.map fst |> List.min
     let maxX = trench |> List.map fst |> List.max
     
     trench
     |> List.groupBy fst
     |> List.sortBy fst
     |> Seq.collect (fun (row, c) ->
         let cols = c |> List.map snd
         
         cols
         |> Array.ofList
         |> Array.sort
         |> intervals
         |> Array.collect (fun (s,e) ->
             //printfn $"%A{row}: %A{s},%A{e}"
             if s = e then
                 [| e |]
             else
                if row > minX && row < maxX then
                    if ((trenchSet |> Set.contains (row + 1, s) && trenchSet |> Set.contains (row - 1, e))
                        || (trenchSet |> Set.contains (row - 1, s) && trenchSet |> Set.contains (row + 1, e))
                        ) 
                        then //trench continues from bottom to top or vice versa (likely :))
                            [| e |] //TODO refactor
                        else
                            Array.empty
                else
                    Array.empty
             )
         |> Array.map (fun col -> (row, col))
         )
     
    
//trenchCoordinates |> breakPoints |> Array.ofSeq |> Array.sort


let fillInsides (trench: (int*int) list) =
    trench
    |> breakPoints
    |> Seq.groupBy fst
    |> Seq.collect (fun (row, points) ->
        points
        |> List.ofSeq
        |> List.map snd
        |> List.sort
        |> List.pairwise
        |> List.mapi (fun i (s,e) ->
            if i % 2 = 0 then
                [ s + 1 .. e - 1 ] |> List.map (fun col -> (row, col))
            else
                [ ]
            )
        |> List.collect id
        )
    
let insidesCounts (trench: (int*int) list) =
    let trenchRows = trench |> List.groupBy fst |> Map.ofList
    
    trench
    |> breakPoints
    |> Seq.groupBy fst
    |> Seq.sortBy fst
    |> Seq.sumBy (fun (row, points) ->
        let pointList = points |> List.ofSeq
        let trenchPoints = trenchRows |> Map.find row |> set
        //printfn $"trenchPoints = %A{trenchPoints}"
        
        pointList
        |> List.map snd
        |> List.sort
        |> List.pairwise
        |> List.mapi (fun i (s,e) ->
            if i % 2 = 0 then
                // [ s + 1 .. e - 1 ]
                // |> List.where (fun col -> trenchPoints |> Set.contains (row, col) |> not)
                // |> List.length
                seq { s + 1 .. e - 1 }
                |> Seq.sumBy (fun col ->
                    if trenchPoints |> Set.contains (row, col) then 0L else 1L)
            else
                0L
            )
        |> List.sum
        |> (fun x -> printfn $"%A{row} filled sum = %A{x}"; x )
        )
    
// trenchCoordinates |> List.length //3152 for PART 1
// trenchCoordinates |> insidesCounts //43893 for PART 1    
    
trenchCoordinates
|> insidesCounts
|> (fun cnt -> cnt + (trenchCoordinates |> List.length |> int64)) 


//46213 too low, 47741 too high, 47045 just right    
        

let printTrenches (writer: TextWriter) (trenches: (int * int) list) =
    let maxY = trenches |> List.map snd |> List.max
    let minY = trenches |> List.map snd |> List.min
    let offset = 1 - minY
    
    trenches
    |> List.groupBy fst
    |> List.sortBy fst
    |> List.iter (fun (row, coords) ->
        
        let cols = coords |> List.map snd |> List.map (fun y -> y + offset)
        let min = cols |> List.min
        if min < 0 then
            failwithf "min = %i" min

        let arr = Array.init (maxY + offset + 3) (fun i ->
            if cols |> List.contains i then '#' else '.' )
        //let line = String(arr)
        let line = $"row    %A{row}: " + String(arr)
        
        writer.WriteLine(line)
        )
    
let w = new StreamWriter(File.OpenWrite("day18-mapB.txt"))
printTrenches w trenchCoordinates
w.Close()
    

// part 1 original    
// trenchCoordinates
// |> fillInsides
// |> Seq.append trenchCoordinates
// //|> Seq.distinct //TODO should not be needed, something is added extra (those that are trenches but also before ending breakpoint)
// |> Seq.sort
// |> Array.ofSeq
// |> Seq.length //46213 too low, 47741 too high, 47045 just right
