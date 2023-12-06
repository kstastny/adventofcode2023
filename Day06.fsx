#time

open System
open System.IO
open System.Text.RegularExpressions

let inputDir = "./inputs"
let input = "input06"
//let input = "testData06"
use file = File.OpenRead(Path.Join(inputDir, input))
use reader = new StreamReader(file)

let numRegex = Regex("[0-9]*")

let getNumbers (x: string) =
    numRegex.Matches(x)
    |> Seq.choose (fun m ->
        if String.IsNullOrWhiteSpace m.Value |> not then
            m.Value |> Int64.Parse |> Some
        else
            None)
    |> Array.ofSeq

let inputArray =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Array.ofSeq
    
//PART 1    
let times = inputArray[0] |> getNumbers  
let records = inputArray[1] |> getNumbers

//PART 2
let times2 = inputArray[0].Replace(" ", "") |> getNumbers  
let records2 = inputArray[1].Replace(" ", "")  |> getNumbers

let getSpeed time = time
    

let recordBeatCounts time record =
    [|1L..time-1L|]
    |> Array.where (fun timeheld ->
        (time - timeheld) * getSpeed timeheld > record
        )
    |> Array.length
    
    
    
let solve t r =
    let countOfBeats =
        Array.zip t r
        |> Array.map (fun (time, record) -> recordBeatCounts time record) 
    
    countOfBeats |> Array.fold (*) 1

$"PART 1: %A{solve times records}"
$"PART 2: %A{solve times2 records2}"