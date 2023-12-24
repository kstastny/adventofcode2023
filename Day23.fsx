#time

open System
open System.Collections.Generic
open System.IO


let file = File.OpenRead("inputs/input23")
//let file = File.OpenRead("inputs/testData23")

let reader = new StreamReader(file)




let inputGrid =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine().ToCharArray()
    }
    |> Array.ofSeq
    
    
type PathSegment = {
    Start: int * int
    End: int * int
    Length: int
    ConnectingCoords: (int * int) list
}

type Path = PathSegment list


// returns neighbours where we can move
let nonForestNeighbours1 (grid: char array array) (x: int, y: int) =
    [
        if x - 1 >= 0 && grid[x - 1][y] <> '#' && grid[x - 1][y] <> 'v' then yield x-1,y
        if x + 1 < grid.Length && grid[x + 1][y] <> '#' && grid[x + 1][y] <> '^' then yield x+1,y
        if y - 1 >= 0 && grid[x][y - 1] <> '#' && grid[x][y - 1] <> '>' then yield x,y-1
        if y + 1 < grid[0].Length && grid[x][y + 1] <> '#' && grid[x][y + 1] <> '<' then yield x,y+1
    ]
    
    
// PART 2    
let nonForestNeighbours (grid: char array array) (x: int, y: int) =
    [
        if x - 1 >= 0 && grid[x - 1][y] <> '#' then yield x-1,y
        if x + 1 < grid.Length && grid[x + 1][y] <> '#' then yield x+1,y
        if y - 1 >= 0 && grid[x][y - 1] <> '#' then yield x,y-1
        if y + 1 < grid[0].Length && grid[x][y + 1] <> '#' then yield x,y+1
    ]    
    


    
// assuming "coords" is always START of path(s)    
let collectPathCoordinates grid (coords: int * int) : PathSegment list =
   
   //following path in one direction
    let rec followPath p c =
        let candidates =
                nonForestNeighbours grid c
                |> List.where (fun x -> p |> List.contains x |> not )
        //printfn $"candidates %A{candidates}, path so far %A{p}"
        //printfn $"candidates %A{candidates}"
        match candidates with
        | [ head ] -> followPath (head::p) head
        | _ -> p //end of path, for whatever reason
       
    let segments =
        match nonForestNeighbours grid coords with
        | [ ] -> [  ]
        | [ head ] ->
           //only one direction - this is one end of path
           followPath [ head; coords ] head |> List.singleton
        | neighbours ->
           neighbours
           |> List.map (fun x -> followPath [x; coords] x)

    segments    
    |> List.map (fun pathSegment ->
        let coords = pathSegment |> List.rev
        let start = coords |> List.head
        let e = coords |> List.last
        
        {
            Start = start
            End = e
            Length = coords |> List.length
            ConnectingCoords = coords |> List.where (fun c -> c <> start && c <> e) 
        }
        )


let parsePathSegments (grid: char array array) (start: int * int)=
    //coordinates of paths, except both ends of path
    let startsToCheck = Queue<int * int>()
    let checkedStarts = HashSet<int * int>()
    
    startsToCheck.Enqueue start
    
    seq {
        while startsToCheck.Count > 0 do
            let s = startsToCheck.Dequeue()
            let paths = collectPathCoordinates grid s
            yield! paths
            paths
                |> List.where (fun p -> checkedStarts.Contains p.End |> not)
                |> List.iter (fun p -> startsToCheck.Enqueue p.End)
            checkedStarts.Add s |> ignore
    }
    |> List.ofSeq
    
// let getAllPaths (segments: PathSegment list) (start: int * int) =
//         
//     let segmentsByStart = segments |> List.groupBy _.Start |> Map.ofList
//     
//     let rec loop visitedStart (s: PathSegment) : Path list =
//         if visitedStart |> Set.contains s.End then
//             [ s ] |> List.singleton //can only end there but not continue
//         else
//             let v2 = visitedStart.Add s.Start
//             //find all paths where we can go
//             match segmentsByStart |> Map.tryFind s.End with
//             | None -> [ s ] |> List.singleton //no paths from here
//             | Some segments ->
//                 segments
//                 |> List.collect (fun nextSegment ->
//                     loop v2 nextSegment
//                     |> List.choose (fun p ->
//                         //filter out returning steps (there may be some to starts of slopes)
//                         match p with
//                         | [] -> [ s ] |> Some
//                         | head::_ ->
//                             if head.ConnectingCoords
//                                |> List.exists (fun c -> s.ConnectingCoords |> List.contains c) then
//                                    None
//                             else
//                                 s::p |> Some
//                         )
//                     )
//                 // segments
//                 // |> List.collect (fun nextSegment ->
//                 //     loop v2 nextSegment
//                 //     |> List.map (fun p -> s::p))    
//     
//     segmentsByStart
//     |> Map.find start
//     |> List.collect (loop Set.empty)
    
    
let getAllPaths (segments: PathSegment list) (target: int * int) (start: int * int) =
        
    let segmentsByStart = segments |> List.groupBy _.Start |> Map.ofList
    
    let rec loop visitedStart (s: PathSegment) : Path list =
        if s.End = target then [ s ] |> List.singleton
        else
        //printfn $"Checking %A{s.Start} - %A{s.End}"
        if visitedStart |> Set.contains s.End then
            [ ] //loop, don't go there
        else
            let v2 = visitedStart.Add s.Start
            //find all paths where we can go
            match segmentsByStart |> Map.tryFind s.End with
            | None ->
          //      printfn $"FOUND END IN %A{s.Start} - %A{s.End}"
                [ s ] |> List.singleton //no paths from the end
            | Some segments ->
                segments
                |> List.collect (fun nextSegment ->
                    loop v2 nextSegment
                    |> List.choose (fun p ->
                        //filter out returning steps (there may be some to starts of slopes)
                        match p with
                        | [] -> [ s ] |> Some
                        //TODO here head is nextSegment!!!
                        | head::_ ->
                            if head.ConnectingCoords
                               |> List.exists (fun c -> s.ConnectingCoords |> List.contains c) then
                                   None
                            else
                                s::p |> Some
                        )
                    )
                // segments
                // |> List.collect (fun nextSegment ->
                //     loop v2 nextSegment
                //     |> List.map (fun p -> s::p))    
    
    segmentsByStart
    |> Map.find start
    |> List.collect (loop Set.empty)    
    
inputGrid[0].Length    
    
//94 for PART 1    
//154 for PART 2
let solve grid start =
    let segments = parsePathSegments grid start |> List.distinct
    let target = grid.Length - 1, grid[0].Length - 2 
    let allPaths = getAllPaths segments start target
    //let target = segments |> List.map (_.End) |> List.maxBy (fun (x,y) -> x + y)
    printfn $"TARGET = %A{target}, segment count = %A{segments |> List.length}"
    
    //allPaths
    //
    let longestPath = 
        allPaths//TODO paths are reversed
        // |> List.where (fun p ->
        //     p |> List.last |> (fun x -> x.End = target)
        //     )
        |> List.map (fun p ->
            p
            |> List.collect (fun s ->
                [
                    yield s.Start
                    yield! s.ConnectingCoords
                    yield s.End
                ]
                )
            |> List.distinct
            |> List.length
            )
        |> List.max    
    longestPath - 1 // -1 because we are already at start
    //
    //

//PART 2 6574, calculated in 53m 29s
//TODO optimize - do not gather all segments, just longest path when looking for target
// TODO optimize 2 - try caching the results, has to be smarter (cannot just include all visitedStarts, since those will differ)
//      but it should be possible to get longest path that does not use any visited  
solve inputGrid (0,1)// |> List.length
//
// let p = solve inputGrid (0,1)
// // p |> List.where (fun x -> x |> List.last |> (fun x -> x.End = (22,21)))
// //
// //     
// // p |> List.length
// //
// //     
// // p |> List.map (fun li -> li |> List.map (fun x -> x.Start, x.End)) |> List.last
// //
// //     
// //     
// p |> List.where (fun x -> x |> List.exists (fun li -> li.End = (22,21)))
// // //
// let segments = parsePathSegments inputGrid (0,1) |> List.distinct
// let paths = getAllPaths segments (0,1) (22,21)
// paths
// |> List.map (fun p ->
//         p
//         |> List.collect (fun s ->
//             [
//                 yield s.Start
//                 yield! s.ConnectingCoords
//                 yield s.End
//             ]
//             )
//         |> List.distinct
//         |> List.length
//         )
//     |> List.max    
//
// paths|> List.map (fun li -> li |> List.map (fun x -> x.Start, x.End)) |> List.head    
    
// let segmentsByStart = segments |> List.groupBy _.Start |> Map.ofList
// segmentsByStart |> Map.find (13,13) |> List.length
// segmentsByStart |> Map.find (22,21) |> List.length
//
// segmentsByStart |> Map.find (22,21)
//
// let target = segments |> List.map (_.End) |> List.maxBy (fun (x,y) -> x + y)
// segments |> List.length
// segments |> List.where (fun s -> s.End = (22,21)) |> List.length
// segments |> List.where (fun s -> s.Start = (22,21)) |> List.length
//
// let segmentsByStart = segments |> List.groupBy _.Start |> Map.ofList
//
// collectPathCoordinates inputGrid (19,19)
//     
// //TODO every segment is here twice, parse is incorrect somehow    
// parsePathSegments inputGrid (0,1) |> List.length
// parsePathSegments inputGrid (0,1) |> List.distinct |> List.length

    
// let aaa = p |> List.head
// aaa |> List


//
// collectPathCoordinates inputGrid (0,1)
// collectPathCoordinates inputGrid (5,3) //missing 6,3
// collectPathCoordinates inputGrid (13,5)

    //|> List.maxBy (fun p -> p |> List.map (_.Length) |> List.sum)

//TODO I don't see why :(  somewhere I have off by one errorc
//let result = longestPath - 1 //PART 1 2626

// PART 2 SAMPLE is 154! so -2 against mine...
// 4619 printed, 4618 TOO LOW, 4619 TOO LOW

// let longestPath2 = 
//     allPaths
//     |> List.where (fun p ->
//         p |> List.last |> (fun x -> x.End = target)
//         )
//     // |> List.map (fun p -> p |> List.map _.Length |> List.sum)
//     // |> List.max
//     |> List.maxBy (fun p -> p |> List.map (_.Length) |> List.sum)
//
// // longestPath |> List.head   
// // longestPath |> List.last
//
// longestPath2
// |> List.collect (_.ConnectingCoords)
// |> List.distinct
// |> List.length
//
// //
// // allPaths
// // |> List.where (fun p ->
//     p |> List.last |> (fun x -> x.End = target)
//     )
// |> List.map (fun p -> p |> List.map _.Length |> List.sum) 

// allPaths |> List.length

