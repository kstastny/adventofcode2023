#time

open System
open System.IO
open System.Text.RegularExpressions
let inputDir = "./AdventOfCode/inputs"
let input = "input03"
//let input = "testData03"
use file = File.OpenRead(Path.Join(inputDir, input))
use reader = new StreamReader(file)


let numRegex = Regex("[0-9]*")


let getNumbers (x: string) =
    numRegex.Matches(x)
    |> Seq.choose (fun m ->
        if String.IsNullOrWhiteSpace m.Value |> not then
            (m.Index, m.Value) |> Some
        else
            None)
    
    

let getParts (schema: string array) =
    let chars = schema |> Array.map (fun x -> x.ToCharArray())
            
    schema
    |> Array.mapi (fun i row -> (i, getNumbers row |> Array.ofSeq))
    |> Array.collect (fun (rowIndex,numbers) ->
        numbers
        |> Array.choose (fun (colIndex, number) ->
            let numS = string number
            
            let symbols =
                seq {
                    for i in [rowIndex-1 ..rowIndex+1] do
                        //!!! numS.Length!
                        for j in [colIndex-1 ..colIndex+numS.Length] do
                            if i > 0 && j > 0 && i < chars.Length && j < chars[i].Length  then
                                let ch = chars[i][j]
                                if ch <> '.' && Char.IsDigit ch |> not then
                                    yield ch
                }
                |> Array.ofSeq
                
            //if rowIndex < 20 then
                // printfn "----------------------"
                // printfn "ROW SYMBOLS: %A for number %s" symbols number
                // printfn "--"
                // if rowIndex > 0 then
                //     printfn "%A" (String(chars[rowIndex - 1]))
                // printfn "%A" (String(chars[rowIndex]))
                // if rowIndex < chars.Length - 1 then
                //     printfn "%A" (String(chars[rowIndex + 1]))
                // printfn "----------------------"                
                
            if symbols |> Seq.exists (fun _ -> true) then
                Some (number |> Int32.Parse)
            else
                None
            )
        )
    
        
let getGears (schema: string array) =
    let chars = schema |> Array.map (fun x -> x.ToCharArray())
            
    schema
    |> Array.mapi (fun i row -> (i, getNumbers row |> Array.ofSeq))
    |> Array.collect (fun (rowIndex,numbers) ->
        numbers
        |> Array.collect (fun (colIndex, number) ->
            let numS = string number
            
            seq {
                for i in [rowIndex-1 ..rowIndex+1] do
                    //!!! numS.Length!
                    for j in [colIndex-1 ..colIndex+numS.Length] do
                        if i > 0 && j > 0 && i < chars.Length && j < chars[i].Length  then
                            let ch = chars[i][j]
                            if ch = '*' then
                                yield (i, j, Int32.Parse number)
            }
            |> Array.ofSeq))        
          
let inputGrid =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Array.ofSeq
    
printfn "part1: %A" (inputGrid |> getParts |> Array.sum)    
printfn "part2: %A"
    (inputGrid
     |> getGears
     |> Array.groupBy (fun (i,j, _) -> i,j)
     |> Array.choose (fun (_, values) ->
         if values.Length = 2 then
             let numbers = values |> Array.map (fun (_,_,n) -> n)
             Some (numbers[0] * numbers[1])
         else
             None
         )
     |> Array.sum
     )
