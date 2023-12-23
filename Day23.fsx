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

let start = (0,1)

// returns neighbours where we can move
let nonForestNeighbours (grid: char array array) (x: int, y: int) =
    [
        if x - 1 >= 0 && grid[x - 1][y] <> '#' && grid[x - 1][y] <> 'v' then yield x-1,y
        if x + 1 < grid.Length && grid[x + 1][y] <> '#' && grid[x + 1][y] <> '^' then yield x+1,y
        if y - 1 >= 0 && grid[x][y - 1] <> '#' && grid[x][y - 1] <> '>' then yield x,y-1
        if y + 1 < grid[0].Length && grid[x][y + 1] <> '#' && grid[x][y + 1] <> '<' then yield x,y+1
    ]
    
nonForestNeighbours inputGrid start    
nonForestNeighbours inputGrid (3,11)
nonForestNeighbours inputGrid (5,3)


// let collectPathCoordinates grid (coords: int * int) =
//    
//    //following path in one direction
//    let rec followPath p c =
//        match nonForestNeighbours grid c with
//        | [ head ] when p |> List.contains head |> not -> followPath (head::p) head
//        | _ -> p
//        
//    match nonForestNeighbours grid coords with
//    | [ ] -> [  ]
//    | [ head ] ->
//        //only one direction - this is one end of path
//        followPath [ coords ] head
//    | [ head ; tail ] -> [] //this is middle of the path - ignore, collect from starts only TODO or it could be crossing with onedirectional entrance
//    | head::tail ->
//        //crossing                                    
    

    
// let parsePathSegments (grid: char array array) =
//     //coordinates of paths, except both ends of path
//     let pathCoords = HashSet<int * int>()
//     //
//     // let collectPathCoordinates (coords: int * int) =
//     //    
//     //    //following path in one direction
//     //    let rec followPath p c =
//     //        match nonForestNeighbours grid c with
//     //        | [ head ] when p |> List.contains head |> not -> loop head::p head
//     //        | _ -> p
//     //        
//     //    match nonForestNeighbours grid coords with
//     //    | [ head ] ->
//     //        //only one direction
//     //        followPath [ coords ] head
//     //    | [ head ; tail ] -> ()
//     //        //two directions 
//     //        followPath [ coords ] tail
//     //        followPath [ coords ] head
//          
//     
//     
//     seq {
//         for i in [0..grid.Length] do
//             for j in [0..grid[0].Length] do
//                 if grid[i][j] <> '#' then //if not in forest
//                     //follow path in both directions until we cannot go anywhere or encounter a crossing
//                     
//                     let pathCoords = collectPathCoordinates  
//     }
//     |> Seq.toArray 




    
// assuming "coords" is always START of path(s)    
let collectPathCoordinates2 grid (coords: int * int) : PathSegment list =
   
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
       
    //TODO rev each result
    let segments =
        match nonForestNeighbours grid coords with
        | [ ] -> [  ]
        | [ head ] ->
           //only one direction - this is one end of path
           followPath [ head; coords ] head |> List.singleton
        | neighbours ->
           neighbours
           |> List.map (followPath [coords])

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
//
// let p = collectPathCoordinates2 inputGrid start
// p |> List.head |> List.length
// p
// nonForestNeighbours inputGrid start
// nonForestNeighbours inputGrid (5,3)

let parsePathSegments (grid: char array array) (start: int * int)=
    //coordinates of paths, except both ends of path
    let startsToCheck = Queue<int * int>()
    let checkedStarts = HashSet<int * int>()
    
    startsToCheck.Enqueue start
    
    seq {
        while startsToCheck.Count > 0 do
            let s = startsToCheck.Dequeue()
            let paths = collectPathCoordinates2 grid s
            yield! paths
            paths
                |> List.where (fun p -> checkedStarts.Contains p.End |> not)
                |> List.iter (fun p -> startsToCheck.Enqueue p.End)
            checkedStarts.Add s |> ignore
    }
    |> List.ofSeq
    
let segments = parsePathSegments inputGrid start
    
    
type Path = PathSegment list


// let getAllPaths (segments: PathSegment list) (start: int * int) =
//         
//     let segmentsByStart = segments |> List.groupBy _.Start |> Map.ofList
//
//     let visitedStart = HashSet<int * int>()
//
//     let rec loop (s: PathSegment) : Path list =
//         if visitedStart.Contains s.End then
//             [ s ] |> List.singleton //can only end there but not continue
//         else
//             visitedStart.Add s.Start |> ignore
//             //find all paths where we can go
//             match segmentsByStart |> Map.tryFind s.End with
//             | None -> [ s ] |> List.singleton //no paths from here
//             | Some segments ->
//                 segments
//                 |> List.collect (fun nextSegment ->
//                     loop nextSegment
//                     |> List.map (fun p -> nextSegment::p)
//                     )
//     
//     segmentsByStart
//     |> Map.find start
//     |> List.collect loop
//


let getAllPaths (segments: PathSegment list) (start: int * int) =
        
    let segmentsByStart = segments |> List.groupBy _.Start |> Map.ofList

    let visitedStart = HashSet<int * int>()

    let rec loop (s: PathSegment) : Path list =
        if visitedStart.Contains s.End then
            [ s ] |> List.singleton //can only end there but not continue TODO allows cycles! fix
        else
            visitedStart.Add s.Start |> ignore
            //find all paths where we can go
            match segmentsByStart |> Map.tryFind s.End with
            | None -> [ s ] |> List.singleton //no paths from here
            | Some segments ->
                segments
                |> List.collect (fun nextSegment ->
                    loop nextSegment
                    |> List.choose (fun p ->
                        //filter out returning steps (there may be some to starts of slopes)
                        match p with
                        | [] -> [ s ] |> Some
                        | head::_ ->
                            if head.ConnectingCoords
                               |> List.exists (fun c -> s.ConnectingCoords |> List.contains c) then
                                   None
                            else
                                s::p |> Some
                        )
                    )
    
    segmentsByStart
    |> Map.find start
    |> List.collect loop
    



let allPaths = getAllPaths segments start
allPaths |> List.head

let target = segments |> List.map (_.End) |> List.maxBy (fun (x,y) -> x + y)


let longestPath = 
    allPaths
    |> List.where (fun p ->
        p |> List.last |> (fun x -> x.End = target)
        )
    |> List.map (fun p -> p |> List.map _.Length |> List.sum)
    |> List.max
    //|> List.maxBy (fun p -> p |> List.map (_.Length) |> List.sum)

//TODO I don't see why :(  somewhere I have off by one error
let result = longestPath - 1 //PART 1 2626

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

