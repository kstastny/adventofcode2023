#time

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text.RegularExpressions

//let file = File.OpenRead("inputs/input18")
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
    

type HorizontalLine =
    {
        Row: int
        LeftCol: int
        RightCol: int
    }
    
type VerticalLine =
    {
        Col: int
        TopRow: int
        BottomRow: int
    }
    
type Line =
    | HorizontalLine of HorizontalLine
    | VerticalLine of VerticalLine
    
    
let getLines instructions =
    instructions
    |> Array.mapFold (fun (x,y) instruction ->
        match instruction.Direction with
        | Right ->
            { Row = x ; LeftCol = y; RightCol = y + instruction.Length } |> HorizontalLine,
            (x, y + instruction.Length)
        | Left ->
            { Row = x ; LeftCol = y - instruction.Length; RightCol = y } |> HorizontalLine,
            (x, y - instruction.Length)
        | Up ->
            { Col = y ; BottomRow = x ; TopRow = x - instruction.Length } |> VerticalLine,
            (x - instruction.Length, y)
        | Down ->
            { Col = y ; BottomRow = x + instruction.Length; TopRow = x } |> VerticalLine,
            (x + instruction.Length, y)
        ) (0,0)
    |> fst
    |> List.ofArray    
    
   
    
let linesLength (lines: Line list) =
    lines
    |> List.sumBy (fun line ->
        match line with
        //NOTE: each line is actually one longer, but we do not want to count corners twice, therefore we would be subtracting one
        | HorizontalLine l -> Math.Abs(l.LeftCol - l.RightCol)
        | VerticalLine l -> Math.Abs(l.TopRow - l.BottomRow)
        )
    |> int64
    
let breakPoints
    (verticalLines: VerticalLine list)
    (horizontalLines: Map<int * int, HorizontalLine>)
    (row: int) =
        verticalLines
        |> List.where (fun l ->
            l.TopRow <= row && l.BottomRow >= row
            )
        |> List.choose (fun l ->
            match l.TopRow, l.BottomRow with
            | top, bottom when top < row && bottom > row ->
                Some l.Col
            | top, bottom when top = row ->
                match horizontalLines |> Map.tryFind (row, l.Col) with //some horizontal line has to be connected there, but not by its right end
                | None -> None
                | Some hl ->
                    //TODO optimize
                    match verticalLines |> List.tryFind (fun l -> l.BottomRow = row && l.Col = hl.LeftCol) with
                    | Some _ -> Some l.Col //there is vertical line in opposite direction
                    | None -> None  
            
            | _ ->
                // the row we are checking is at the bottom of this line 
                match horizontalLines |> Map.tryFind (row, l.Col) with //some horizontal line has to be connected there, but not by its right end
                | None -> None
                | Some hl ->
                    //TODO optimize
                    match verticalLines |> List.tryFind (fun l -> l.TopRow = row && l.Col = hl.LeftCol) with
                    | Some _ -> Some l.Col //there is vertical line in opposite direction
                    | None -> None 
            )


    
let filledCount (lines: Line list) =
    let verticalLines =
        lines |> List.choose (fun line ->
            match line with
            | VerticalLine l -> Some l
            | _ -> None
        )
        |> List.sortBy (_.Col)
        
    //printfn $"vertical lines: %A{verticalLines}"
    let horizontalLineByRightCoordinates =
        lines |> List.choose (fun line ->
            match line with
            | HorizontalLine l -> Some l
            | _ -> None)
        //|> List.groupBy (fun l -> (l.Row, l.RightCol))
        |> List.map (fun l -> ((l.Row, l.RightCol), l))
        |> Map.ofList
        
    let horizontalLineByRow =
        lines |> List.choose (fun line ->
            match line with
            | HorizontalLine l -> Some l
            | _ -> None)
        |> List.groupBy _.Row
        |> Map.ofList        
    
    //printfn $"horizontal lines: %A{horizontalLineByRightCoordinates}"        
        
    let minX = verticalLines |> List.map _.TopRow |> List.min
    let maxX = verticalLines |> List.map _.BottomRow |> List.max
    printfn "minX = %A, maxX = %A" minX maxX
    
    seq {
        for row in [minX .. maxX] do
            //for each row, find breakpoints
            yield breakPoints verticalLines horizontalLineByRightCoordinates row
            |> (fun b ->
                // if row = -242 then
                //     printfn $"BREAKPOINTS: %A{b}"
                b)
            |> Seq.pairwise
            |> Seq.mapi (fun i (s,e) ->
                // map breakpoints pairwise, for each even number, add the difference
                //printfn $"%A{row}: %A{s},%A{e}"
                if i % 2 = 0 then
                    //remove length of all horizontal lines between start and end breakpoints
                    let lengthToRemove =
                        horizontalLineByRow
                        |> Map.tryFind row
                        |> Option.map (List.sumBy (fun l ->
                            if l.LeftCol > s && l.RightCol < e then
                                Math.Abs(l.RightCol - l.LeftCol) + 1 |> int64
                            elif l.LeftCol > s && l.RightCol = e then
                                //for the one ending in E, only length-1 has to be removed because E is a breakpoint and that is not counted anyway
                                Math.Abs(l.RightCol - l.LeftCol) |> int64 
                            else
                                0L))
                        |> Option.defaultValue 0L
                    let x = int64 e - int64 s - 1L - lengthToRemove
                    //printfn $"%A{row} (cols %A{s},%A{e}) lengthToRemove = %A{lengthToRemove}, filled = %A{x}"
                    x
                    //if there is horizontal line to end breakpoint, remove its length from the count
                    // match horizontalLineByRightCoordinates |> Map.tryFind(row, e) with
                    // | None -> e - s - 1 |> int64  // the breakpoint is not counted 
                    // | Some l ->
                    //     let aaa = e - s - Math.Abs(l.RightCol - l.LeftCol) - 1 |> int64 // - 1 because the line is one longer than coordinate difference
                    //     //printfn $"aaa %A{aaa}, %A{l}"
                    //     aaa
                else
                    0L                
                )
            |> Seq.sum
            //|> (fun x -> printfn $"%A{row} filled sum = %A{x}"; x )
            
    }
    |> Seq.sum
    
    


let solve (parse: string -> Instructions) (file: string) =
    
    let reader = new StreamReader(file)

    let instructions =
        seq {
            while reader.EndOfStream |> not do
                yield reader.ReadLine() |> parse
        }
        |> Array.ofSeq
      
    reader.Close()
    
    let lines = instructions |> getLines
    
    //lines |> linesLength // 3152, is OK for PART 1
    
    // let filled = filledCount lines
    // filled //should be 24 for test data in PART 1
    
    //filledCount lines // should be 43893 in PART 1
    linesLength lines + filledCount lines 
     
     
//solve parseInstructions "inputs/testData18"    // 62 (38 + 24)
//solve parseInstructions "inputs/input18"      // 47045 (3152 + 43893)
//solve parseInstructions2 "inputs/testData18"  // 952408144115   
solve parseInstructions2 "inputs/input18"     