#time

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO


type Vector = Int64 * Int64 * Int64

let addVectors (v1: Vector) (v2: Vector) =
    let x1, y1, z1 = v1
    let x2, y2, z2 = v2
    x1+x2, y1+y2, z1+z2
    
let multiplyVector (v1: Vector) (n: int64) =
    let x1, y1, z1 = v1
    x1*n, y1*n, z1*n
    
  

type Hailstone = {
    Position: Vector
    Velocity: Vector
}

// y = ax+b
type Line2D = {
    A: float
    B: float
}

(*
a = (y1 - y2) / (x1 - x2)
b = y2 - a*x2 = y1 - a*x1
*)
let hailstoneLine (hailstone: Hailstone) =
    let x1, y1, _ = hailstone.Position
    let x2, y2, _ = addVectors hailstone.Position hailstone.Velocity
    
    let a = (float y1 - float y2) / (float x1 - float x2)
    
    {
        A = a
        B = float y1 - a * float x1
    }
    
let precision = 0.00001

let floatEquals (x1: float) (x2: float) =
    Math.Abs(x1 - x2) < precision
    
(*
x = (b1 - b2) / (a2 - a1)
*)    
let lineIntersection (line1: Line2D) (line2: Line2D) =
    if floatEquals line1.A line2.A then
        None
    else
        let x =
            (line1.B - line2.B) / (line2.A - line1.A)
        let y = line1.A * x + line1.B
        Some (x, y)
    
    
// let l1 = { A = 1.; B = 1. }
// let l2 = { A = -0.5; B = 22.5 }
//
// let getY (line: Line2D) x =
//     line.A * x + line.B


let parseHailstone (row: string) =
    let parseVector (v: string) =
        let parts =
            v.Split(',', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        Int64.Parse parts[0], Int64.Parse parts[1], Int64.Parse parts[2]
        
    let positionAndVector = row.Split('@', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    {
        Position = positionAndVector[0] |> parseVector
        Velocity = positionAndVector[1] |> parseVector 
    }
    


//NOTE: does not really check if the hailstone intersects, just if it would intersect, if it goes in that direction
let isFuturePosition (hailstone: Hailstone) (x: float, _: float ) =
    let posX, _, _ = hailstone.Position 
    let vecX, _, _ = hailstone.Velocity
    (vecX > 0 && float posX < x) //X is increasing, possibly heading towards position 
    || (vecX < 0 && float posX > x) //X is decreasing, possibly heading towards position
    
    
    

let solve1 filename testAreaMin testAreaMax =

    let file = File.OpenRead filename

    use reader = new StreamReader(file)
    
    let hailstones =
            seq {
                while reader.EndOfStream |> not do
                    yield reader.ReadLine() |> parseHailstone
            }
            |> Array.ofSeq

    let lines = hailstones |> Array.map hailstoneLine

    seq {
        for i in [0..lines.Length - 1] do
            for j in [i+1 .. lines.Length - 1] do
                (hailstones[i], hailstones[j], lineIntersection lines[i] lines[j])
    }
    |> Seq.choose (fun (line1, line2, intersection) ->
        match intersection with
        | Some i -> Some (line1, line2, i)
        | None -> None
        )
    |> Seq.where (fun (_, _, (x,y)) ->
        //check if crossed in area
        x > testAreaMin && x < testAreaMax
        && y > testAreaMin && y < testAreaMax
        )
    |> Seq.where (fun (h1, h2, intersection) ->
        // check if crossed in THE FUTURE for both hailstones
        isFuturePosition h1 intersection
        && isFuturePosition h2 intersection
    )
    |> Seq.length
            
    
    
solve1 "inputs/testData24" 7. 27.
solve1 "inputs/input24" 200000000000000. 400000000000000.


// PART 2
//checks if line intersects all other lines
let intersectsAll (lines: Line2D array) (line: Line2D) =
    lines
    |> Array.forall (fun x -> lineIntersection line x |> Option.isSome)

let subtractVectors (v1: Vector) (v2: Vector) =
    let x1, y1, z1 = v1
    let x2, y2, z2 = v2
    x1-x2, y1-y2, z1-z2  


let moveInTime (hailstone: Hailstone) (t: int64) =
    { hailstone with
        Position = multiplyVector hailstone.Velocity t |> addVectors hailstone.Position  }    

let h5 = parseHailstone "19, 13, 30 @ -2, 1, -2"
let h6 = parseHailstone "12, 31, 28 @ -1, -2, -1"

let h4 = parseHailstone "20, 25, 34 @ -2, -2, -4"

let h4Collision = moveInTime h4 4
let h5Collision = moveInTime h5 5
let h6Collision = moveInTime h6 6


multiplyVector h6Collision.Velocity -5

let rock =
    let v = subtractVectors h5Collision.Position h4Collision.Position
    {
        Position = h4Collision.Position
        Velocity = v 
    }
    
moveInTime rock -4 // (24L, 13L, 10L) (-3L, 1L, 2L) } 



//let solve2 filename =
    
//let filename = "inputs/testData24"
let filename = "inputs/input24"

let file = File.OpenRead filename

use reader = new StreamReader(file)

let hailstones =
        seq {
            while reader.EndOfStream |> not do
                yield reader.ReadLine() |> parseHailstone
        }
        |> Array.ofSeq
        
let lines = hailstones |> Array.map hailstoneLine

//lines.Length

let isValid (hailStones: Hailstone array) (rock: Hailstone) =
    let livingHailstones = HashSet<Hailstone>()
    hailStones |> Array.iter (fun x -> livingHailstones.Add x |> ignore)
    let rockLine = rock |> hailstoneLine
    
    let maxTime = 500
    let mutable time = 0
    let mutable futureValid = true
    while livingHailstones.Count > 0 && time < maxTime && futureValid do
        time <- time + 1
        let rockInTime = moveInTime rock time
        if time % 20 = 0 then
            futureValid <-
                livingHailstones
                |> Seq.forall (fun h ->
                    match lineIntersection rockLine (hailstoneLine h) with
                    | None -> false
                    | Some i -> isFuturePosition rockInTime i 
                    )
        
        let rockPosition = rockInTime.Position
        livingHailstones
        |> Seq.iter (fun h ->
            // if time = 1 then
            //     printfn $"rockPosition = %A{rockPosition}, hail position = %A{(moveInTime h time).Position}"
            
            if (moveInTime h time).Position = rockPosition then
                livingHailstones.Remove h |> ignore
            )
   // livingHailstones |> Seq.map id |> Array.ofSeq
    
    livingHailstones.Count = 0
    
    
//just a quick validation    
// let isPossible (hailStones: Hailstone array) (rock: Hailstone) =
//     let livingHailstones = HashSet<Hailstone>()
//     hailStones |> Array.iter (fun x -> livingHailstones.Add x |> ignore)
//     
//     let maxTime = 200
//     let mutable time = 0
//     
//     //TODO validate that all hailstones are in the future of rock!
//     while livingHailstones.Count > 0 && time < maxTime do
//         time <- time + 1
//         let rockPosition = (moveInTime rock time).Position
//         livingHailstones
//         |> Seq.iter (fun h ->
//             if (moveInTime h time).Position = rockPosition then
//                 livingHailstones.Remove h |> ignore
//             )
//     
//     livingHailstones.Count < 150   
    
    

let candidates previousCollision nextCollision =
    let stopwatch = Stopwatch()
    stopwatch.Start()
    // let firstPosition = 299
    // let lastPosition = 300
    
    // 299 and 300 found nothing
    // let previousCollision = lines.Length - 1
    // let nextCollision = lines.Length
    
    seq {
        for i in [0..lines.Length - 1] do
            for j in [i+1 .. lines.Length - 1] do
                let iInNextCollision = moveInTime hailstones[i] nextCollision
                let iInPrevCollision = moveInTime hailstones[i] previousCollision
                let jInNextCollision = moveInTime hailstones[j] nextCollision
                let jInPrevCollision = moveInTime hailstones[j] previousCollision
                
                let c1 =
                    {
                        Position = iInPrevCollision.Position
                        Velocity = subtractVectors jInNextCollision.Position iInPrevCollision.Position 
                    }
                yield moveInTime c1 (int64 -previousCollision)
                    
                let c2 = 
                    {
                        Position = jInPrevCollision.Position
                        Velocity = subtractVectors iInNextCollision.Position jInPrevCollision.Position 
                    }                    
                yield moveInTime c2 (int64 -previousCollision)
    }
    |> Seq.indexed
    |> Seq.where (fun (i,c) ->
        if i % 10000 = 0 then printfn $"Checking candidate %A{i}, time elapsed %A{stopwatch.Elapsed}"
        
        intersectsAll lines (hailstoneLine c)
        //&& isPossible hailstones c
        )
    |> Seq.where (fun (_,c) ->
        isValid hailstones c
        )
    |> Seq.map (fun (_, c) ->
        printfn $"%A{c} seems valid"
        c
        )
    |> Seq.tryHead
    
    
//candidates 4 5

// let rockInTime = moveInTime rock -4 
//
// isValid hailstones rockInTime
//     
//TODO possible optimization - move everything 300x and see what is relatively close    
//candidates 299 300        

for i in [150..160] do
    for step in [1..5] do
        let prev = i
        let next = i + step
        printfn $"Checking steps %i{prev} and %i{next}"
        
        let result = candidates prev next
        printfn $"Steps %i{prev} and %i{next} result = %A{result}"

        
    
    