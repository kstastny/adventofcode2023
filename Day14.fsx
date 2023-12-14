#time

open System

open System.IO

let file = File.OpenRead("inputs/input14")
//let file = File.OpenRead("inputs/testData14")
//let file = File.OpenRead("inputs/testData14-load")

type Grid = char array array


let reader = new StreamReader(file)

let inputGrid =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine().ToCharArray()
    }
    |> Array.ofSeq



    

//NOTE: new grid
let rotateCounterClockwise (grid: Grid) =
    let rowCount = grid.Length
    [|
        for i in [0..grid.Length - 1] do
            [|
                for j in [0..grid[0].Length - 1] do
                    yield grid[j][rowCount - 1 - i]                
            |]
    |]
    
    
let copy (grid: Grid) =
    grid |> Array.copy
    
//TODO immutable solution
let flip (grid: Grid) (i1,j1) (i2,j2) =
    let tmp = grid[i1][j1]
    grid[i1][j1] <- grid[i2][j2]
    grid[i2][j2] <- tmp    

// let moveNorth (grid: Grid) (i: int, j: int) =
//     let tmp = grid[i - 1][j]
//     grid[i - 1][j] <- grid[i][j]
//     grid[i][j] <- tmp

let moveNorth (grid: Grid) (i: int, j: int) = flip grid (i, j) (i - 1, j)
let moveWest (grid: Grid) (i: int, j: int) = flip grid (i, j) (i, j - 1)
let moveSouth (grid: Grid) (i: int, j: int) = flip grid (i, j) (i + 1, j)
let moveEast (grid: Grid) (i: int, j: int) = flip grid (i, j) (i, j + 1)

    

    
let rotateClockwise (grid: Grid) =
    grid |> rotateCounterClockwise |> rotateCounterClockwise |> rotateCounterClockwise   
    

//NOTE: modify in place!
let slideNorth (grid: Grid) =
    for i in [1..grid.Length - 1] do
        for j in [0..grid[0].Length - 1] do
            let mutable targetRow = i - 1
            while targetRow >= 0
                  && grid[targetRow][j] = '.'
                  && grid[targetRow + 1][j] = 'O' //only move round stones
                  do
                moveNorth grid (targetRow + 1, j)
                targetRow <- targetRow - 1
    inputGrid
    
let slideSouth (grid: Grid) =
    for i in [0..grid.Length - 2] |> List.rev do
        for j in [0..grid[0].Length - 1]  do
            let mutable targetRow = i + 1
            while targetRow < grid.Length
                  && grid[targetRow][j] = '.'
                  && grid[targetRow - 1][j] = 'O' //only move round stones
                  do
                moveSouth grid (targetRow - 1, j)
                targetRow <- targetRow + 1
    inputGrid
    
let slideEast (grid: Grid) =
    for i in [0..grid.Length - 1] do
        for j in [0..grid[0].Length - 2] |> List.rev do
            let mutable targetCol = j + 1
            while targetCol < grid[j].Length
                  && grid[i][targetCol] = '.'
                  && grid[i][targetCol - 1] = 'O' //only move round stones
                  do
                moveEast grid (i, targetCol - 1)
                targetCol <- targetCol + 1
    inputGrid
    
let slideWest (grid: Grid) =
    for i in [0..grid.Length - 1] do
        for j in [1..grid[0].Length - 1] do
            let mutable targetCol = j - 1
            while targetCol >= 0
                  && grid[i][targetCol] = '.'
                  && grid[i][targetCol + 1] = 'O' //only move round stones
                  do
                moveWest grid (i, targetCol + 1)
                targetCol <- targetCol - 1
    inputGrid      

let calculateLoad (grid: Grid) =
    let northSouthDistance = grid.Length
    [
        for i in [0..grid.Length - 1] do
            for j in [0..grid[0].Length - 1] do
                if grid[i][j] = 'O' then
                    yield northSouthDistance - i
    ]
    |> List.sum
            
                
//PART 1: slideNorth inputGrid |> calculateLoad
// slideNorth inputGrid
// slideSouth inputGrid
// slideEast inputGrid
// slideWest inputGrid
//
// inputGrid

//1 billion... TODO find out some pattern and how to calculate. 
let cycleCount = 1000000000
// let cycleCount = 10

let differences (x: int array) = 
    x
    |> Array.pairwise
    |> Array.map (fun (x,y) -> y - x)
    

let loads, _ =
    seq { 0..10000 }
    |> Seq.mapFold (fun state cycle ->
        //printfn $"%A{state}"
        let newState = state |> slideNorth |> slideWest |> slideSouth |> slideEast |> copy
        
        printfn $"%i{cycle}: load after cycle = {newState |> calculateLoad}"
        //printfn $"%A{newState}"
        
        newState |> calculateLoad, newState
        ) inputGrid

let loadsArray = loads |> Array.ofSeq
let diffs = loadsArray |> differences

let testArray = loadsArray[500..]


//TODO period - skip a couple until it stabilizes, then add until we are back at 0

let rec getPeriod n =
    printfn "period %i" n
    if n > (testArray.Length / 2) then
        failwith "period not found"
    elif testArray[0..n - 1] = testArray[n .. 2*n - 1] then
        n
    else
        getPeriod (n+1)
        
        
let period = getPeriod 2

printfn $"%i{loadsArray[(cycleCount - 1) % period + 50*period]}"
    
      
    


//TODO find repeating period automatically (for testData the period is 7)
// (1000000000 - 1) % 7 = 5 -> result is the sixth in period

        

// |> calculateLoad 
        //BUT: load is 64 after 1000000000!!

 //(10 - 1) % 7

//1 billion... TODO find out some pattern and how to calculate. 
//let cycleCount = 1000000000
// let cycleCount = 10
// seq { 0..3 }
// |> Seq.fold (fun state cycle ->
//     printfn $"%A{state}"
//     let north = state |> slideNorth
//     let west = north |> rotateClockwise |> slideNorth
//     let south = west |> rotateClockwise |> slideNorth
//     let east = south |> rotateClockwise |> slideNorth
//     printfn $"%i{cycle}: north = {calculateLoad north}, west = {calculateLoad west}, south = {calculateLoad south}, east = {calculateLoad east}"
//     printfn $"%i{cycle}: after cycle = {east |> rotateClockwise |> calculateLoad}"
//     east |> rotateClockwise //return original orientation
//     ) inputGrid
// |> calculateLoad
