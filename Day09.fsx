#time

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions


let numRegex = Regex("[-]?[0-9]*")

let getNumbers (x: string) =
    numRegex.Matches(x)
    |> Seq.choose (fun m ->
        if String.IsNullOrWhiteSpace m.Value |> not then
            m.Value |> Int64.Parse |> Some
        else
            None)
    |> Array.ofSeq



let file = File.OpenRead("inputs/input09")
//let file = File.OpenRead("inputs/testData09")

let reader = new StreamReader(file)

let inputArray =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Seq.map getNumbers
    |> Array.ofSeq
    
    
let differences (x: int64 array) = 
    x
    |> Array.pairwise
    |> Array.map (fun (x,y) -> y - x)

    
//let reportValues = inputArray[0]

let intermediateArrays reportValues = 
    let rec loop arr =
        seq {
            yield arr
            if arr |> Array.forall (fun i -> i = 0L) |> not then
                yield! arr |> differences |> loop
        }
    reportValues |> loop |> Array.ofSeq        

let nextValue (reportValues: int64 array) =
    
    let interArrays = reportValues |> intermediateArrays
    
    let tails =
        interArrays
        |> Array.map Array.last
        |> Array.rev
        

    let nextValue =
        tails[1..]
        |> Array.fold
            (fun state t ->
                state + t
            )
            tails[0]
    nextValue
    
    
//let reportValues = inputArray[2]    
let previous (reportValues: int64 array) =

    let interArrays = reportValues |> intermediateArrays
    
    let heads =
        interArrays
        |> Array.map Array.head
        |> Array.rev
        

    let nextValue =
        heads[1..]
        |> Array.fold
            (fun state t ->
                t - state
            )
            heads[0]
    nextValue
    
    
    
    
inputArray
|> Array.map nextValue
|> Array.sum
|> printfn "PART 1: %A"
    
inputArray
|> Array.map previous
|> Array.sum
|> printfn "PART 2: %A"