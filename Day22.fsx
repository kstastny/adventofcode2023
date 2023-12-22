#time

open System
open System.IO



type Coordinates = int * int * int

type Brick = {
    BrickNumber: int
    OccupiedSpace: Coordinates list
}



let parseBrick i (row: string) =
    let parseCoordinates (coord: string) =
        let c = coord.Split(',', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        c[0] |> Int32.Parse, c[1] |> Int32.Parse, c[2] |> Int32.Parse
    
    let parts = row.Split('~')
    let s = parts[0] |> parseCoordinates
    let e = parts[1] |> parseCoordinates
    
    {
      BrickNumber = i
      OccupiedSpace =
        [
            let mutable x, y, z = s
            let xE, yE, zE = e
            yield x,y,z //start
            //assumption - LEFT part contains LOWER coords
            while x < xE do
                x <- x + 1
                yield x,y,z 
            while y < yE do
                y <- y + 1
                yield x,y,z 
            while z < zE do
                z <- z + 1
                yield x,y,z                     
            
            yield x,y,z //end
        ] |> List.distinct
    }
    
    
// parseBrick "1,0,1~1,2,1"    
// parseBrick "0,0,2~2,0,2"
//parseBrick "0,2,3~2,2,2"

let intersects (brick1: Brick) (brick2: Brick) =
    brick1.OccupiedSpace
    |> List.exists (fun x -> brick2.OccupiedSpace |> List.exists (fun y -> x = y))
    
    
let moveDown (brick: Brick) =
    { brick with
        OccupiedSpace =
            brick.OccupiedSpace |> List.map (fun (x,y,z) -> (x,y, z - 1)) }
    
//move brick as far down as possible
let fallDown (bricks: Brick array) (brick: Brick) =
    
    let otherBricks = bricks |> Array.where (fun x -> x <> brick)
    
    let rec loop b =
        //printfn $"%A{b}"
        let potentialPosition = b |> moveDown
        if  potentialPosition.OccupiedSpace |> List.exists (fun (_,_,z) -> z <= 0)
            || (otherBricks |> Array.exists (intersects potentialPosition))
            then
            //    printfn $"aa %A{otherBricks |> Array.where (intersects potentialPosition)}"
                b
        else
            loop potentialPosition
            
    let newBrick = loop brick
    
    otherBricks |> Array.insertAt 0 newBrick
        
    
    
let compact (bricks:  Brick array) =
    bricks
    |> Array.sortBy (fun x -> x.OccupiedSpace |> List.map (fun (_,_,z) -> z))
    |> Array.fold (fun (newBricks: Brick array) brick ->
        //printfn $"Falling down %A{brick.BrickNumber}"
        fallDown newBricks brick
        ) bricks
    

let getBricks filename =
    let file = File.OpenRead(filename)
    use reader = new StreamReader(file)
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Seq.mapi parseBrick
    |> Array.ofSeq
    
    
    
let isSupportedBy (bricks: Brick array) (brick: Brick) =
    let otherBricks = bricks |> Array.where (fun x -> x <> brick)
    let down = brick |> moveDown
    otherBricks |> Array.where (intersects down)
    
    

//let bricks = getBricks "inputs/testData22"

let bricks = getBricks "inputs/input22"

let fallenBricks = bricks |> compact


let singleSupports = 
    fallenBricks
    |> Array.map (fun x -> x, isSupportedBy fallenBricks x |> Array.map (_.BrickNumber))
    |> Array.where (fun (x, supports) -> supports.Length = 1)
    |> Array.collect (fun (x, s) -> s)
    |> set
    
let allBricks = fallenBricks |> Array.map (fun x -> x.BrickNumber) |> set    
    
printfn $"PART 1: {(Set.difference allBricks singleSupports).Count}"
    
//sanity check
// let occupiedSpace =
//     fallenBricks
//     |> Seq.collect (_.OccupiedSpace)
//     |> Array.ofSeq
//     
// occupiedSpace.Length = (occupiedSpace |> Array.distinct |> Array.length)
