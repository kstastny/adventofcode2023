#time

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type GridCell =
    | UpDownSplitter
    | LeftRightSplitter
    | ForwardMirror
    | BackwardMirror
    | Empty
    
type BeamDirection =
    | Up
    | Down
    | Left
    | Right
    
type Beam = { X: int ; Y: int ; Direction: BeamDirection }    
    
let cellType = function
    | '|' -> UpDownSplitter
    | '-' -> LeftRightSplitter
    | '/' -> ForwardMirror
    | '\\' -> BackwardMirror
    | '.' -> Empty
    | x -> failwithf $"Unexpected char: %c{x}"
    



let file = File.OpenRead("inputs/input16")
//let file = File.OpenRead("inputs/testData16")

let reader = new StreamReader(file)

let inputGrid =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Seq.map (fun x -> x.ToCharArray() |> Array.map cellType)
    |> Array.ofSeq
    

let next (grid: GridCell array array) (beam: Beam) =
    seq {
        match beam.Direction, grid[beam.X][beam.Y] with
        | _, Empty -> yield beam.Direction
        | Up, UpDownSplitter -> yield Up
        | Up, LeftRightSplitter ->
            yield Left
            yield Right
        | Up, ForwardMirror -> yield Right
        | Up, BackwardMirror -> yield Left
        | Down, UpDownSplitter -> yield Down
        | Down, LeftRightSplitter ->
            yield Left
            yield Right
        | Down, ForwardMirror -> yield Left
        | Down, BackwardMirror -> yield Right            
        | Right, LeftRightSplitter -> yield Right
        | Right, UpDownSplitter ->
            yield Up
            yield Down
        | Right, ForwardMirror -> yield Up
        | Right, BackwardMirror -> yield Down
        | Left, LeftRightSplitter -> yield Left
        | Left, UpDownSplitter ->
            yield Up
            yield Down
        | Left, ForwardMirror -> yield Down
        | Left, BackwardMirror -> yield Up
    }
    |> Seq.map (fun nextCell ->
     //   printfn $"%A{nextCell}"
        match nextCell with
        | Up -> { Direction =  nextCell; X = beam.X - 1 ; Y = beam.Y }
        | Down -> { Direction =  nextCell; X = beam.X + 1 ; Y = beam.Y }
        | Left -> { Direction =  nextCell; X = beam.X ; Y = beam.Y - 1 }
        | Right -> { Direction =  nextCell; X = beam.X ; Y = beam.Y + 1 }
        )
    |> Seq.where (fun cell ->
        cell.X >= 0 && cell.Y >= 0 && cell.X < grid.Length && cell.Y < grid[0].Length
        )


{ X = 1; Y = 0; Direction = Right }
|> next inputGrid
|> Seq.toArray
    
let lightBouncing grid beam =
    
    //track visited and not generate there
    let checkedBeams = HashSet<Beam>()
    
    let rec loop x =
        if checkedBeams.Contains x then
            Seq.empty
        else
            checkedBeams.Add x |> ignore
            seq {
                for nextBeam in next grid x do
                    yield nextBeam
                    yield! loop nextBeam
            }
            
   // printfn "checkedBeams.Count = %i" checkedBeams.Count
        
    loop beam
    |> Seq.map (fun x ->
      //  printfn "%A" x
        x
        )
    //|> Seq.take 100
    |> Seq.map (fun x -> (x.X, x.Y))
    |> Seq.distinct
    |> Seq.length
    
// PART 1    
// { X = 0; Y = 0; Direction = Right }
// |> lightBouncing inputGrid


//{ X = 0; Y = 3; Direction = Down } |> lightBouncing inputGrid


// PART 2    
seq {
    for i in [0..inputGrid.Length - 1] do
        yield { X = i; Y = 0; Direction = Right } |> lightBouncing inputGrid
        yield { X = i; Y = inputGrid.Length - 1; Direction = Left } |> lightBouncing inputGrid
    for j in [0..inputGrid[0].Length - 1] do
        yield { X = 0; Y = j; Direction = Down } |> lightBouncing inputGrid
        yield { X = inputGrid.Length - 1; Y = j; Direction = Up } |> lightBouncing inputGrid
} |> Seq.max