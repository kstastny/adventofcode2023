#time

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type GridCell =
    | NorthSouth
    | EastWest
    | NorthEast
    | NorthWest
    | SouthWest
    | SouthEast
    | Ground
    | Animal
    
let cellType = function
    | '|' -> NorthSouth
    | '-' -> EastWest
    | 'L' -> NorthEast
    | 'J' -> NorthWest
    | '7' -> SouthWest
    | 'F' -> SouthEast
    | '.' -> Ground
    | 'S' -> Animal



let file = File.OpenRead("inputs/input10")
//let file = File.OpenRead("inputs/testData10")
//let file = File.OpenRead("inputs/testData10b") // 4 enclosed
//let file = File.OpenRead("inputs/testData10c") // 8 enclosed
//let file = File.OpenRead("inputs/testData10d") // 10

let reader = new StreamReader(file)

let inputGrid =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Seq.map (fun x -> x.ToCharArray() |> Array.map cellType)
    |> Array.ofSeq
    
let connections = function
    | NorthSouth -> ['N'; 'S']
    | EastWest  -> ['E'; 'W']
    | NorthEast  -> ['N'; 'E']
    | NorthWest -> ['N'; 'W']
    | SouthWest  -> ['S'; 'W']
    | SouthEast  -> ['S'; 'E']
    | Animal -> ['N';'S';'E';'W']
    | _ -> []
    
    
//TODO fix - wrong for NW above SE   
let connectingPipes ((x1,y1), c1) ((x2,y2), c2) =
    let conn1 = connections c1
    let conn2 = connections c2
    //left, top, right, down
    if x1 = x2 then
        if y1 < y2  then
            //2 is EAST
            (conn1 |> List.contains 'E') && (conn2 |> List.contains 'W')
        else
            //2 is WEST
            (conn1 |> List.contains 'W') && (conn2 |> List.contains 'E')
    else
        if x1 < x2 then
            //2 is SOUTH
            (conn1 |> List.contains 'S') && (conn2 |> List.contains 'N')
        else
            (conn1 |> List.contains 'N') && (conn2 |> List.contains 'S')
    
connectingPipes ((3,0), NorthSouth) ((4,0), NorthEast)   
    
//x row, y column    
let orthogonalNeighbours (grid: GridCell array array) (rowIndex, colIndex) =
    seq {
        for i, j in
            [
                (rowIndex - 1, colIndex)
                (rowIndex + 1, colIndex)
                (rowIndex, colIndex + 1)
                (rowIndex, colIndex - 1)
            ] do
            if i >= 0 && j >= 0 && i < grid.Length && j < grid[i].Length then
                yield ((i,j), grid[i][j])  
    }
    
    
let animalCoordinates =
    seq {
        for i in [0 ..inputGrid.Length - 1] do
            for j in [0 ..inputGrid[0].Length - 1] do
                if inputGrid[i][j] = Animal then 
                    yield i,j
    }
    |> Seq.head
    
    
orthogonalNeighbours inputGrid animalCoordinates



let connectingHorizontally row x y =
    connectingPipes
        ((row,x),inputGrid[row][x])
        ((row,y),inputGrid[row][y])
        
let connectingVertically column x y =
    connectingPipes
        ((x, column),inputGrid[x][column])
        ((y, column),inputGrid[y][column])        
    
    
let visited = HashSet<int * int>()
visited.Add animalCoordinates

let mutable stepNum = 0
let mutable coord = animalCoordinates
let mutable pathCells = [| inputGrid[animalCoordinates |> fst][animalCoordinates |> snd] |]

let mutable p = [| coord, inputGrid[coord |> fst][coord |> snd] |]

while p |> Array.length > 0 do
    let paths =
        //p |> Array.collect
        p[0] |>
               (fun (currentCoord, currCell) ->
            orthogonalNeighbours inputGrid currentCoord
            |> Seq.where (fun (neighbCoord,cell) ->
                // printfn $"n %A{neighbCoord} %A{cell}"
                // printfn $"connectingPipes (currentCoord, currCell) (neighbCoord, cell) = %b{connectingPipes (currentCoord, currCell) (neighbCoord, cell)}"
                connectingPipes (currentCoord, currCell) (neighbCoord, cell)
                && (visited.Contains neighbCoord |> not)
                )
            |> Array.ofSeq
            )
    paths |> Array.iter (fun (c,_) -> visited.Add c |> ignore)
    //visited.Add (fst p[0]) |> ignore
    
 //   printfn $"paths = %A{paths}"
    
    stepNum <- stepNum + 1
    p <- paths
    
//    
(stepNum + 1) / 2   
    
    
    
let intervals (x: int array) =
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
    
    
let intervals2 row (x: int array) =
    if x.Length = 0 then
        Array.empty
    else
        seq {
            let mutable iStart = x |> Array.head
            let mutable iEnd = x |> Array.head
            for n in x[1..] do
                //if n = iEnd + 1 then
                if connectingHorizontally row iEnd n then
                    iEnd <- n
                else
                    yield iStart, iEnd
                    iStart <- n
                    iEnd <- n
            yield iStart, iEnd
        }
        |> Array.ofSeq    
    
    
    
[|(1, 1); (4, 4); (6, 6); (9, 9)|]
|> Array.pairwise
|> Array.mapi (fun i ((s1,e1),(s2,e2)) ->
        if i % 2 = 0 then
            //take those that are not part of pipes
            [e1..s2]
            |> List.where (fun col -> visited.Contains(6, col) |> not)
            |> List.length
        else
            0
    )



[|(1, 1); (2, 2); (8, 8); (9, 9)|]
|> Array.pairwise
|> Array.mapi (fun i ((s1,e1),(s2,e2)) ->
        if i % 2 = 0 then
            //take those that are not part of pipes
            [e1..s2]
            |> List.where (fun col -> visited.Contains(3, col) |> not)
            |> List.length
        else
            0
    )


    
let horizontalIntervals =     
    visited
    |> Seq.groupBy fst
    |> Seq.map (fun (row, c) ->
        row, c |> Seq.map snd |> Array.ofSeq |> Array.sort |> intervals2 row
        )
    |> Array.ofSeq
    
horizontalIntervals
|> Array.map (fun (row, rowIntervals) ->
        row, rowIntervals
        |> Array.pairwise
        |> Array.mapi (fun i ((s1,e1),(s2,e2)) ->
            if i % 2 = 0 then
                //take those that are not part of pipes
                [e1..s2]
                |> List.where (fun col -> visited.Contains(6, col) |> not)
                |> List.length
            else
                0
        )
    )


//TODO collect "possibly enclosed" in ROWS, THEN dtto for COLUMNS
    
// //handle row
// t
// |> Seq.map (fun (row,columns) ->
//     row,
//     columns
//     |> Array.pairwise
//     |> Array.mapi (fun i ((s1,e1),(s2,e2)) ->
//             if i % 2 = 0 then
//                 //take those that are not part of pipes
//                 [e1..s2]
//                 |> List.where (fun col -> visited.Contains(row, col) |> not)
//                 |> List.length
//             else
//                 0
//         )    
//     
//     )
// |> Array.ofSeq
// |> Array.where (fun (_, cols) -> cols |> Array.sum > 0) //for checking results
//
// |> Array.map snd
// |> Array.collect id
// |> Array.sum

//inputGrid[0].Length

let intervalsColumn col (x: int array) =
    if x.Length = 0 then
        Array.empty
    else
        seq {
            let mutable iStart = x |> Array.head
            let mutable iEnd = x |> Array.head
            for n in x[1..] do
                //if n = iEnd + 1 then
                if connectingVertically col iEnd n then
                    iEnd <- n
                else
                    yield iStart, iEnd
                    iStart <- n
                    iEnd <- n
            yield iStart, iEnd
        }
        |> Array.ofSeq  

let verticalIntervals =     
    visited
    |> Seq.groupBy snd
    |> Seq.map (fun (col, c) ->
        col, c |> Seq.map fst |> Array.ofSeq |> Array.sort |> intervalsColumn col
        )
    |> Array.ofSeq
    

//TODO collect "possibly enclosed" in ROWS, THEN dtto for COLUMNS
    
let possibleFromRows =    
    horizontalIntervals
    |> Seq.map (fun (row,columns) ->
        columns
        |> Array.where (fun (x,y) -> x = y)
        |> Array.pairwise
        |> Array.mapi (fun i ((s1,e1),(s2,e2)) ->
                if i % 2 = 0 then
                    //take those that are not part of pipes
                    [e1..s2]
                    |> List.where (fun col -> visited.Contains(row, col) |> not)
                    |> List.map (fun col -> row, col)
                    |> Array.ofList
                else
                    Array.empty
            )
        |> Array.concat
        )
    |> Array.ofSeq
    |> Array.concat


let possibleFromCols =
    verticalIntervals
    |> Seq.map (fun (col,rows) ->
        rows
        |> Array.where (fun (x,y) -> x = y)
        |> Array.pairwise
        |> Array.mapi (fun i ((s1,e1),(s2,e2)) ->
                if i % 2 = 0 then
                    //take those that are not part of pipes
                    [e1..s2]
                    |> List.where (fun row -> visited.Contains(row, col) |> not)
                    |> List.map (fun row -> row, col)
                    |> Array.ofList
                else
                    Array.empty
            )
        |> Array.concat
        )
    |> Array.ofSeq
    |> Array.concat    


let possibleFromColsS = possibleFromCols |> set 
let possibleFromRowsS = possibleFromRows |> set

let s = Set.intersect possibleFromColsS possibleFromRowsS
s.Count //does not work

//TODO https://stackoverflow.com/questions/217578/how-can-i-determine-whether-a-2d-point-is-within-a-polygon

let horMap = horizontalIntervals |> Map.ofArray
let verMap = verticalIntervals |> Map.ofArray


// let count =
//     seq {
//         for i in [0 ..inputGrid.Length - 1] do
//             match horMap |> Map.tryFind i with
//             | Some interval ->
//                 for j in [0 ..inputGrid[0].Length - 1] do
//                     let hits =
//                         if visited.Contains(i,j) |> not then
//                             let h = 
//                                 interval
//                                 |> Array.where (fun (iStart, iEnd) -> iEnd < j )
//                                 |> Array.collect (fun (iStart, iEnd) -> [|iStart; iEnd |])
//                                 |> Array.distinct
//                             if i = 1 && j = 10 then
//                                 printfn $"hits = %A{h}"
//                             h |> Array.length
//                         else
//                             0
//                             
//                     if hits % 2 = 1 then
//                         printfn $"{i}, {j}, hits = {hits}"
//                         yield i,j
//             | None -> ()
//     }
//     |> Array.ofSeq
//
// let count2 =
//     seq {
//         for i in [0 ..inputGrid.Length - 1] do
//             for j in [0 ..inputGrid[0].Length - 1] do
//             match verMap |> Map.tryFind j with
//             | Some interval ->
//                     let hits =
//                         if visited.Contains(i,j) |> not then
//                             let h = 
//                                 interval
//                                 |> Array.where (fun (iStart, iEnd) -> iEnd < i )
//                                 |> Array.collect (fun (iStart, iEnd) -> [|iStart; iEnd |])
//                                 |> Array.distinct
//                             if i = 1 && j = 10 then
//                                 printfn $"hits = %A{h}"
//                             h |> Array.length
//                         else
//                             0
//                             
//                     if hits % 2 = 1 then
//                         printfn $"{i}, {j}, hits = {hits}"
//                         yield i,j
//             | None -> ()
//     }
//     |> Array.ofSeq
//     
// let aaa = Set.intersect (set count) (set count2)
// aaa.Count

// let count3 =
//     seq {
//         for i in [0 ..inputGrid.Length - 1] do
//             match horMap |> Map.tryFind i with
//             | Some interval ->
//                 for j in [0 ..inputGrid[0].Length - 1] do
//                     let hits =
//                         if visited.Contains(i,j) |> not then
//                             let h = 
//                                 interval
//                                 |> Array.map snd
//                                 |> Array.where (fun x -> x < j )
//                             h |> Array.length
//                         else
//                             0
//                             
//                     if hits % 2 = 1 && hits < interval.Length then
//                         printfn $"{i}, {j}, hits = {hits}"
//                         yield i,j
//             | None -> ()
//     }
//     |> Array.ofSeq
//
// count3 |> Array.length
//
// let count4 =
//     seq {
//         for i in [0 ..inputGrid.Length - 1] do
//             for j in [0 ..inputGrid[0].Length - 1] do
//             match verMap |> Map.tryFind j with
//             | Some interval ->
//                     let hits =
//                         if visited.Contains(i,j) |> not then
//                             let h = 
//                                 interval
//                                 |> Array.map fst
//                                 |> Array.where (fun x -> x < i )
//                             h |> Array.length
//                         else
//                             0
//                             
//                     if hits % 2 = 1 then
//                         printfn $"{i}, {j}, hits = {hits}"
//                         yield i,j
//             | None -> ()
//     }
//     |> Array.ofSeq
//     
// let aaa = Set.intersect (set count3) (set count4)
// aaa.Count


let bothCornersNorthOrBothSouth cell1 cell2 =
    match cell1, cell2 with
    | NorthEast, NorthWest -> true
    | NorthWest, NorthEast -> true
    | SouthEast, SouthWest -> true
    | SouthWest, SouthEast -> true
    //TODO this is incorrect, depends on what is there 
    // | Animal, _ -> true
    // | _, Animal -> true

    //input data - animal is NORTH WEST TODO determine animal position automatically
    | Animal, NorthEast -> true
    | NorthEast, Animal -> true        
    
    | _ -> false



let count5 =
    seq {
        for i in [0 ..inputGrid.Length - 1] do
            match horMap |> Map.tryFind i with
            | Some interval ->
                for j in [0 ..inputGrid[0].Length - 1] do
                    let hits =
                        if visited.Contains(i,j) |> not then
                            let h = 
                                interval
                                |> Array.where (fun (iStart, iEnd) ->
                                    iEnd < j
                                    && (iStart = iEnd
                                        || (bothCornersNorthOrBothSouth (inputGrid[i][iStart]) (inputGrid[i][iEnd])) |> not)
                                    )
                            h |> Array.length
                        else
                            0
                            
                    if hits % 2 = 1 && hits < interval.Length then
                        //printfn $"{i}, {j}, hits = {hits}"
                        yield i,j
            | None -> ()
    }
    |> Array.ofSeq
    

// 521 is too low
count5.Length

