#time

open System
open System.Text
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let file = File.OpenRead("inputs/input13")
//let file = File.OpenRead("inputs/testData13")

type Grid = char array array


let reader = new StreamReader(file)

let inputGrids =
    let input = 
        seq {
            while reader.EndOfStream |> not do
                yield reader.ReadLine()
        }
        |> Array.ofSeq
    let separators =
        input
        |> Array.indexed
        |> Array.where (fun (_,x) -> String.IsNullOrEmpty x)
        |> Array.map fst
    
    seq {
        yield input[..separators[0] - 1]
        yield! separators |> Array.pairwise |> Array.map (fun (s, e) -> input[s+1..e - 1])
        yield input[separators[separators.Length - 1] + 1 ..]
    }
    |> Seq.map (fun x -> x |> Array.map (_.ToCharArray()))
    |> Array.ofSeq
    
    
//TODO inverse rows and cols
let inverseGrid (grid: Grid) =
    [|
        for j in [0..grid[0].Length - 1] do
            [|
                for i in [0..grid.Length - 1] do
                    yield grid[i][j]                
            |]
    |]


//possible reflection AFTER specified index
let possibleReflection (row: char array) index =
    if index < 0 || index > row.Length - 2
        then false
    else
        let left = row[0..index] |> Array.rev
        let right = row[index+1..row.Length - 1] 
        //printfn $"left %A{left}, right = %A{right}"
        let a, b =
            if left.Length > right.Length then
                left[0..right.Length - 1] |> List.ofArray,
                right |> List.ofArray
            else
                left |> List.ofArray,
                right[0..left.Length - 1] |> List.ofArray
        a = b
        // match left.Length, right.Length with
        // | x, y when x = y -> x = y
        // | x, y when x > y ->
        //     let a = left[0..right.Length - 1] |> List.ofArray
        //     let b = right |> List.ofArray
        //     printfn $"{a} = {b}"
        //     left[0..right.Length - 1] = right
        // | _ ->
        //     printfn $"{left} = {right[0..left.Length - 1]}"
        //     left = right[0..left.Length - 1]
      
    
let horizontalReflections (grid: Grid) =
    grid
    |> Array.map (fun row ->
        [0..row.Length - 1]
        |> List.where (possibleReflection row)
        |> set
        )
    |> Array.reduce Set.intersect
    //NOTE: there should be just one
    |> Seq.tryHead
    

    
// let row = inputGrids[0][0]
// [0..row.Length - 1] |> List.map (possibleReflection row)
//
// possibleReflection row 4
//
// inputGrids[0] |> horizontalReflections

inputGrids
|> Array.map (fun grid ->
    match grid |> horizontalReflections with
    | Some n -> n+1
    | None ->
        match grid |> inverseGrid |> horizontalReflections with
        | Some n -> 100*(n+1)
        | None ->
            printfn "ERROR?"
            0
    )
|> Array.sum