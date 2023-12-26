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
    
let divideVector (v1: Vector) (n: int64) =
    let x1, y1, z1 = v1
    x1/n, y1/n, z1/n

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
            
    
    
// solve1 "inputs/testData24" 7. 27.
// solve1 "inputs/input24" 200000000000000. 400000000000000.


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


let rock2 =
    let v = subtractVectors h6Collision.Position h4Collision.Position
    {
        Position = h4Collision.Position
        Velocity = divideVector v 2 
    }
    
moveInTime rock2 -4    



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
    
    let maxTime = 5000
    let mutable time = 0
    let mutable futureValid = true
    while livingHailstones.Count > 0 && time < maxTime && futureValid do
        time <- time + 1
        let rockInTime = moveInTime rock time
        
        let rockPosition = rockInTime.Position
        livingHailstones
        |> Seq.iter (fun h ->
            // if time = 1 then
            //     printfn $"rockPosition = %A{rockPosition}, hail position = %A{(moveInTime h time).Position}"
            
            if (moveInTime h time).Position = rockPosition then
                livingHailstones.Remove h |> ignore
            )
        
        //check if all remaining are still possible targets
        if time = 1 || (time % 20 = 0) then
            futureValid <-
                livingHailstones
                |> Seq.forall (fun h ->
                    match lineIntersection rockLine (hailstoneLine h) with
                    | None -> false
                    | Some i ->
                        // if rock.Velocity = (-3,1,2) then
                        //     printfn $"rockInTime %i{time}, position {rock.Position}, intersection %A{i}, h = %A{h}, isFuturePosition = {isFuturePosition rockInTime i} "
                        isFuturePosition rockInTime i 
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
    
    
//step - how many nanoseconds ellapsed between the assumed collisions
let candidates previousCollision nextCollision =
    let step = nextCollision - previousCollision
    let stopwatch = Stopwatch()
    stopwatch.Start()
    
    seq {
        for i in [0..lines.Length - 1] do
            for j in [i+1 .. lines.Length - 1] do
                let iInNextCollision = moveInTime hailstones[i] nextCollision
                let iInPrevCollision = moveInTime hailstones[i] previousCollision
                let jInNextCollision = moveInTime hailstones[j] nextCollision
                let jInPrevCollision = moveInTime hailstones[j] previousCollision
                
                let v1 = subtractVectors jInNextCollision.Position iInPrevCollision.Position
                let c1 =
                    {
                        Position = iInPrevCollision.Position
                        Velocity = divideVector v1 step
                            
                    }
                yield moveInTime c1 (int64 -previousCollision)
                    
                let v2 = subtractVectors iInNextCollision.Position jInPrevCollision.Position
                let c2 = 
                    {
                        Position = jInPrevCollision.Position
                        Velocity =  divideVector v2 step 
                    }                    
                yield moveInTime c2 (int64 -previousCollision)
    }
    |> Seq.indexed
    |> Seq.where (fun (i,c) ->
        // if i % 50000 = 0 then
        //     printfn $"times between [{previousCollision}] and [{nextCollision}], checking candidate %A{i}, time elapsed %A{stopwatch.Elapsed}"
        
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
    
    
// NOTE: it should be enough to find TWO times when the collisions happen and the candidates will find solution    
// candidates 4 5
// candidates 4 6
// candidates 1 6
// candidates 1 3

// let rockInTime = moveInTime rock -4 
//
// isValid hailstones rockInTime
//     
//TODO possible optimization - move everything 300x and see what is relatively close    
//candidates 299 300        

//NOTE STEP other than 1ms does not work!
// for i in [150..160] do
//     for step in [1..5] do
//         let prev = i
//         let next = i + step
//         printfn $"Checking steps %i{prev} and %i{next}"
//         
//         let result = candidates prev next
//         printfn $"Steps %i{prev} and %i{next} result = %A{result}"



// let result = 
//     Array.init 250 (fun i -> Array.init 250 (fun j -> (i + 1, i + j + 2)))
//     |> Array.concat
//     |> Array.Parallel.choose (fun (prev, next) ->
//        // printfn $"Checking steps %i{prev} and %i{next}"
//             
//         let result = candidates prev next
//         printfn $"Steps %i{prev} and %i{next} result = %A{result}"
//         result
//         )
// printfn $"RESULT = %A{result}"


let guessRock (h1: Hailstone) (h2: Hailstone) time1 time2 =
    let step = time2 - time1
    let h1Collision = moveInTime h1 time1
    let h2Collision = moveInTime h2 time2
    let v = subtractVectors h2Collision.Position h1Collision.Position
    let rockInCollistion =
        {
            Position = h2Collision.Position
            Velocity = divideVector v step
        }
    moveInTime rockInCollistion -time2
    
//validates that rock hits hailstone
let validateRock (hailstone: Hailstone) (rock: Hailstone) =
    match lineIntersection (hailstoneLine hailstone) (hailstoneLine rock) with
    | None -> false
    | Some intersection -> 
        if isFuturePosition hailstone intersection |> not
            then false
        elif isFuturePosition rock intersection |> not
            then false
        else
            let intersectionX, intersectionY = intersection
            let rockX, _, _ = rock.Position
            let rockVx, _, _ = rock.Velocity
            //just for validation
            let hailX, _, _ = hailstone.Position
            let hailVx, _, _ = hailstone.Velocity
            //TODO what if the time is not integer? that would be bad :(
            let rockTime = (intersectionX - float rockX) /  float rockVx
            let hailTime = (intersectionX - float hailX) /  float hailVx
        //    printfn $"AAAAAA %A{intersectionX}, %A{intersectionY}, rockTime %A{rockTime}, hailTime %A{hailTime}"
            let rockCollision = moveInTime rock (int64 rockTime)
            let hailCollision = moveInTime hailstone (int64 rockTime)
            rockCollision.Position = hailCollision.Position
            
        
    
    
// Position = (24L, 13L, 10L), Velocity = (-3L, 1L, 2L) }
// collision 4 - (12,17)
// T = (12-24)/-3 = -12/-3 = -4
// guessRock h5 h6 5 6 |> validateRock h4
// guessRock h4 h5 4 5 |> validateRock h6
// guessRock h4 h6 4 6 |> validateRock h5
//
// guessRock h4 h6 1 6 |> validateRock h5
        
let hailstoneDistances =
    hailstones
    |> Array.allPairs hailstones
    |> Array.where (fun (x,y) -> x <> y)
    |> Array.map (fun (x,y) ->
        let xx, xy, xz = x.Position
        let yx, yy, yz = y.Position
        (Math.Sqrt(Math.Pow(float xx - float yx, 2) + Math.Pow(float xy- float yy, 2) + Math.Pow(float xz - float yz, 2)),
         Math.Abs (xx - xy),
         Math.Abs (xy - yy), x, y)
    )
    |> Array.sortBy (fun (a,b,c,d,e) ->
        a
        )
    
//hailstoneDistances[0]
        
let findRock msChecked =
    let stopwatch = Stopwatch()
    stopwatch.Start()        
    
    //TODO get some that are close and one that is farther to validate
    // let h1 = hailstones[0]
    // let h2 = hailstones[1]
    let a,b,c,d,e = hailstoneDistances[0]
    let h1 = d
    let h2 = e
    let hControl = hailstones[2]
    //let msChecked = 10000
    let mutable cnt = 0
    
    guessRock h1 h2 1 20000000
    
    //TODO there is more than one solution for testData :( for the first equation checking...
    //guessRock h1 h2 5 2 |> validateRock hControl
    
    //guessRock h1 h2 5 3 |> validateRock hControl
    // let a =
    //     Seq.allPairs (Seq.init 5 (fun i -> i+1)) (Seq.init 5 (fun i-> i+1))
    //     |> Array.ofSeq

    Seq.allPairs (Seq.init msChecked (fun i -> i+1)) (Seq.init msChecked (fun i -> i+1))
    |> Seq.choose (fun (i,j) ->
        //if ((i + j) % 10000 = 0) then
        cnt <- cnt + 1
        if (cnt % 1000000 = 0) then
            printfn $"Time %A{stopwatch.Elapsed} checking %i{i},%i{j}"
        if i = j then
            None
        else
            let r = guessRock h1 h2 i j
            if r |> validateRock hControl then
                printfn $"i = %i{i}, j = %i{j}"
                Some r
                    else
                None
        )
    //TODO optimize isValid
    |> Seq.where (fun rock ->
        printfn $"Checking suspicious rock: %A{rock}"
        isValid hailstones rock
        ) 
//    |> Seq.toArray
    

// let arr = 
//     findRock 100000
//     |> Seq.map (fun r ->
//         printfn $"ROCK %A{r}"
//         r
//     )
//     |> Array.ofSeq
//
// let sums = arr |> Array.map (fun r ->
//     let x,y,z = r.Position
//     x+y+z
//     )

    

//TOO HIGH - idiot me, that was control stone, not result
//293577250654200L + 176398758803665L + 272206447651388L
    
(*
val hControl: Hailstone =
  { Position = (293577250654200L, 176398758803665L, 272206447651388L)
    Velocity = (-17L, 101L, 26L) }

*)    
    