open System
open System.IO
let inputDir = "./AdventOfCode/inputs"
let input = "input01"
use file = File.OpenRead(Path.Join(inputDir, input))
use reader = new StreamReader(file)

let calibrationValue (x: string) =
    let min, max =
        x.ToCharArray()
        |> Array.fold (fun (min, max) ch ->
            if Char.IsDigit ch then
                let digit = Int32.Parse(string ch)
                match min, max with
                | None, None -> Some digit, Some digit
                | Some min, Some _ -> Some min, Some digit
                | _ -> (min, max)
            else
                min, max
            )
            (None, None)
    match min, max with
    | Some x, Some y -> 10 * x + y |> Some
    | _ -> None
    

let wordNumbers = [
    "one", 1
    "1", 1
    "two", 2
    "2", 2
    "three", 3
    "3", 3
    "four", 4
    "4", 4
    "five", 5
    "5", 5
    "six", 6
    "6", 6
    "seven", 7
    "7", 7
    "eight", 8
    "8", 8
    "nine", 9
    "9", 9
]


    
let calibrationValue2 (x: string) =
    let min =
        wordNumbers
        |> List.choose (fun (word,num) ->
            match x.IndexOf word with
            | x when x > -1 -> Some (x, num)
            | _ -> None
            )
        |> List.minBy fst
        |> snd
        
        
    let max =
        wordNumbers
        |> List.choose (fun (word,num) ->
            match x.LastIndexOf word with
            | x when x > -1 -> Some (x, num)
            | _ -> None
            )
        |> List.maxBy fst
        |> snd
    
    10*min + max
        
    
    
seq {
    while reader.EndOfStream |> not do
        yield reader.ReadLine() |> calibrationValue2
        // let line = reader.ReadLine()
        // let value = calibrationValue2 line
        // printfn $"For %s{line} the value is %A{value}"
        // yield value
        
}
//|> Seq.choose id
|> Seq.sum
