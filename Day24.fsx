#time

open System
open System.Collections.Generic
open System.IO


type Vector = Int64 * Int64 * Int64

let addVectors (v1: Vector) (v2: Vector) =
    let x1, y1, z1 = v1
    let x2, y2, z2 = v2
    x1+x2, y1+y2, z1+z2

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
    
    
// getY l1 14.3333    
// getY l2 14.3333    
//
// lineIntersection l1 l2
// lineIntersection l2 l1

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
    
//parseHailstone "19, 13, 30 @ -2,  1, -2"

//NOTE: does not really check if the hailstone intersects, just if it would intersect, if it goes in that direction
let isFuturePosition (hailstone: Hailstone) (x: float, y: float ) =
    let posX, _, _ = hailstone.Position 
    let vecX, _, _ = hailstone.Velocity
    (vecX > 0 && float posX < x) //X is increasing, possibly heading towards position 
    || (vecX < 0 && float posX > x) //X is decreasing, possibly heading towards position 

let solve1 filename testAreaMin testAreaMax =

    //let file = File.OpenRead "inputs/testData24"
    let file = File.OpenRead filename

    use reader = new StreamReader(file)
    
    let hailstones =
            seq {
                while reader.EndOfStream |> not do
                    yield reader.ReadLine() |> parseHailstone
            }
            |> Array.ofSeq

    let lines =
        hailstones |> Array.map hailstoneLine
        
    // printfn $"%A{lines[0]}"
    // printfn $"%A{lines[1]}"
    // lineIntersection lines[0] lines[1]
    // lineIntersection lines[1] lines[0]
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
    |> Seq.where (fun (line1, line2, (x,y)) ->
        //check if crossed in area
        x > testAreaMin && x < testAreaMax
        && y > testAreaMin && y < testAreaMax
        )
    |> Seq.where (fun (line1, line2, intersection) ->
        // check if crossed in THE FUTURE for both hailstones
        isFuturePosition line1 intersection
        && isFuturePosition line2 intersection
    )
    |> Seq.length
            
    
    
solve1 "inputs/testData24" 7. 27.
solve1 "inputs/input24" 200000000000000. 400000000000000.
    