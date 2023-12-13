#time

open System

open System.IO

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
        let a, b =
            if left.Length > right.Length then
                left[0..right.Length - 1] |> List.ofArray,
                right |> List.ofArray
            else
                left |> List.ofArray,
                right[0..left.Length - 1] |> List.ofArray
        a = b

let horizontalReflectionsAll (grid: Grid) =
    grid
    |> Array.map (fun row ->
        [0..row.Length - 1]
        |> List.where (possibleReflection row)
        |> set
        )
      
let horizontalReflectionsAll2 except (grid: Grid) =
    grid
    |> Array.map (fun row ->
        [0..row.Length - 1]
        |> List.where (possibleReflection row)
        |> set
        )
    |> Array.reduce Set.intersect
    |> (fun x -> x.Remove except)
    |> Seq.tryHead
    
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
    
  
let flip = function
    | '#' -> '.'
    | _ -> '#'
    

let horizontalReflections2 ignored (grid: Grid) =
    [
        for i in [0..grid.Length - 1 ] do
            for j in [0..grid[0].Length - 1] do
                grid[i][j] <- grid[i][j] |> flip
                yield grid |> horizontalReflectionsAll2 ignored           
                //flip back
                grid[i][j] <- grid[i][j] |> flip
    ]
    |> List.choose id
    |> List.where (fun i -> i <> ignored)
    |> List.tryHead
    
let getReflectionNumber grid =
    match grid |> horizontalReflections with
    | Some n -> n+1
    | None ->
        match grid |> inverseGrid |> horizontalReflections with
        | Some n -> 100*(n+1)
        | None ->
            printfn "ERROR?"
            0    

// part  
inputGrids
|> Array.map getReflectionNumber
|> Array.sum
|> printfn "PART 1: %A"


inputGrids
|> Array.mapi (fun i grid ->
    match grid |> horizontalReflections with
    | Some n ->
        //fixed data
        match grid |> horizontalReflections2 n with
        | Some m when m <> n -> m+1
        | _ ->
            match grid |> inverseGrid |> horizontalReflections2 -1 with
            | Some m -> 100*(m+1)
            | None ->
                printfn $"ERROR 2A? for row %i{i}"
                //return original
                getReflectionNumber grid 
    | None ->
        match grid |> inverseGrid |> horizontalReflections with
        | Some n ->
            //fixed data
            match grid |> inverseGrid |> horizontalReflections2 n with
            | Some m when m <> n -> 100*(m+1)
            | _ ->
                match grid |> horizontalReflections2 -1 with
                | Some m -> m+1
                | None ->
                    printfn $"ERROR 2B? for row %i{i}"
                    getReflectionNumber grid
        | None ->
            printfn "ERROR?"
            0
    )
|> Array.sum
|> printfn "PART 2: %A"

