#time

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let file = File.OpenRead("inputs/input17")
//let file = File.OpenRead("inputs/testData17")

let reader = new StreamReader(file)

let inputGrid =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Seq.map (fun x -> x.ToCharArray() |> Array.map (fun x -> Int32.Parse(string x)))
    |> Array.ofSeq
    
type Direction =
    | Up
    | Down
    | Left
    | Right
    | Start
    
    
// TODO better way to calculate, having all these states separate means we have to calculate up to gridCellCount * 3 (distance traveled) * 4 (directions from) states    
// StraightDistanceTraveled max 3. Distance as in blocks, not heat lost    
type State = { X: int ; Y: int ; EnteredFrom: Direction; StraightDistanceTraveled: int32 }

//TODO search from start, breadth first
// for each state - set shortest path to map. mark as "visited" (not in Dijkstra sense?)
// find all followers and recursively call them with heatLoss so far

let nextStates (grid: int array array) (state: State) =
    [
        if state.X - 1 >= 0 && state.EnteredFrom <> Left then
            { X = state.X - 1
              Y = state.Y
              EnteredFrom = Right
              StraightDistanceTraveled = if state.EnteredFrom = Right then state.StraightDistanceTraveled + 1 else 1
            }
        if state.X + 1 < grid.Length && state.EnteredFrom <> Right then
            { X = state.X + 1
              Y = state.Y
              EnteredFrom = Left
              StraightDistanceTraveled = if state.EnteredFrom = Left then state.StraightDistanceTraveled + 1 else 1
            }
        if state.Y - 1 >= 0 && state.EnteredFrom <> Up then
            { X = state.X
              Y = state.Y - 1
              EnteredFrom = Down
              StraightDistanceTraveled = if state.EnteredFrom = Down then state.StraightDistanceTraveled + 1 else 1
            }
        if state.Y + 1 < grid[0].Length  && state.EnteredFrom <> Down then
            { X = state.X
              Y = state.Y + 1
              EnteredFrom = Up
              StraightDistanceTraveled = if state.EnteredFrom = Up then state.StraightDistanceTraveled + 1 else 1
            }               
    ]
    |> List.where (fun x -> x.StraightDistanceTraveled <= 3)
    
let s = { X = 0; Y = 0; EnteredFrom = Start; StraightDistanceTraveled = 0 }
nextStates inputGrid s

let dijsktra (grid: int array array) (startX: int) (startY: int) =
    let start = { X = startX; Y = startY; EnteredFrom = Start; StraightDistanceTraveled = 0 } 
    let visited = HashSet<State>()
    
    let distances = Dictionary<State, int32>()
    distances[start] <- 0
    
    
    let targetDistances = HashSet<State>()
    let unvisitedNodes = HashSet<State>()
    unvisitedNodes.Add s |> ignore
    
    let rec loop s =
        // if visited.Count % 1000 = 0 then 
        //     printfn $"visited.Count = {visited.Count}, s = {s}"
        //printfn $"visited.Count = {visited.Count}, s = {s}"
        for checkedState in nextStates grid s do
            if visited.Contains checkedState |> not then
                unvisitedNodes.Add checkedState |> ignore
                let distance = distances[s] + grid[checkedState.X][checkedState.Y]
                match distances.TryGetValue checkedState with
                | false, _ -> distances[checkedState] <- distance
                | true, d when d > distance -> distances[checkedState] <- distance
                | _ -> ()
        visited.Add s |> ignore
        unvisitedNodes.Remove s |> ignore
        //also mark the turns as visited
        // if s.StraightDistanceTraveled = 1 then
        //     if s.
        
        if s.X = grid.Length - 1 && s.Y = grid[0].Length - 1 then
            targetDistances.Add s |> ignore
        
        //let unvisitedNodes = distances |> Seq.where (fun kv -> visited.Contains kv.Key |> not)
        if unvisitedNodes |> Seq.isEmpty |> not
            //stop when we have two targets
           // && visited |> Seq.where (fun x -> x.X = grid.Length - 1 && x.Y = grid[0].Length - 1 ) |> Seq.length < 2
            || targetDistances.Count < 2
            then
            //TODO better collection, sorted 
            let nextNode = unvisitedNodes
                           |> Seq.map (fun x -> x, distances[x])
                           |> Seq.minBy snd                       
            loop (fst nextNode)
    loop start
    
    distances// |> Seq.where (fun x -> x.Key.X = grid.Length - 1 && x.Key.Y = grid[0].Length - 1)
                
                
                
inputGrid.Length                       
inputGrid[0].Length - 1                       
                
                
// let d = dijsktra inputGrid 0 0
//
// printfn "distances counted = %A" d.Count

dijsktra inputGrid 0 0
|> (fun d -> printfn $"distances counted = %A{d.Count}"; d)
|> Seq.where (fun x -> x.Key.X = inputGrid.Length - 1 && x.Key.Y = inputGrid[0].Length - 1)
|> Array.ofSeq
|> Array.minBy _.Value
|> printfn "PART 1: %A"

// d |> Seq.take 4
//  
// d |> Seq.map (_.Key.X) |> Seq.max    
// d |> Seq.map (_.Key.Y) |> Seq.max    
//   
// d |> Seq.where (fun x -> x.Key.X = 11 && x.Key.Y = 11) |> Seq.minBy _.Value
// d |> Seq.where (fun x -> x.Key.X = 0 && x.Key.Y = 3) |> Seq.minBy _.Value
// d |> Seq.where (fun x -> x.Key.X = 1 && x.Key.Y = 3) |> Seq.minBy _.Value
// // breadth first search, for each node mark shortest distance. If visited and shortest distance -> stop in that node    