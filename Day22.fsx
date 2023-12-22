#time

open System
open System.Collections.Generic
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
        let potentialPosition = b |> moveDown
        if  potentialPosition.OccupiedSpace |> List.exists (fun (_,_,z) -> z <= 0)
            || (otherBricks |> Array.exists (intersects potentialPosition))
            then
                b
        else
            loop potentialPosition
            
    let newBrick = loop brick
    
    otherBricks |> Array.insertAt 0 newBrick
        
    
    
let compact (bricks:  Brick array) =
    bricks
    |> Array.sortBy (fun x -> x.OccupiedSpace |> List.map (fun (_,_,z) -> z))
    |> Array.fold (fun (newBricks: Brick array) brick ->
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
    
    
let solve filename = 

    let bricks = getBricks filename

    printfn "Compacting"
    let fallenBricks = bricks |> compact
    printfn "Compacted"
    

    let supportedBy =
        fallenBricks
        |> Array.collect (fun x -> isSupportedBy fallenBricks x |> Array.map (fun y -> x.BrickNumber, y.BrickNumber))
        |> Array.groupBy fst
        |> Map.ofArray
        |> Map.map (fun _ v -> v |> Array.map snd)
        
        
    let supporting =
        fallenBricks
        |> Array.collect (fun x -> isSupportedBy fallenBricks x |> Array.map (fun y -> y.BrickNumber, x.BrickNumber))
        |> Array.groupBy fst
        |> Map.ofArray
        |> Map.map (fun _ v -> v |> Array.map snd)
        
    let singleSupports =
        supportedBy
        |> Map.toSeq
        |> Seq.where (fun (k, v) -> v.Length = 1)
        |> Seq.map snd
        |> Seq.concat
        |> Set
        
    let allBricks = fallenBricks |> Array.map (_.BrickNumber) |> set
    printfn $"PART 1: {(Set.difference allBricks singleSupports).Count}"
    
    
    // PART 2 for each brick, determine how many bricks would fall. Sum number of other bricks that would fall
    supporting //if brick is not supporting anything, nothing will fall by disintegrating it
    |> Map.toSeq
    |> Seq.map (fun (brick, supportedBricks) ->
        //count of supported bricks
        let bricksToCheck = Queue<int>()
        let fallenBricks =  HashSet<int>()
        fallenBricks.Add brick |> ignore
        supportedBricks |> Array.iter bricksToCheck.Enqueue
        while bricksToCheck.Count > 0 do
            let checkedBrick = bricksToCheck.Dequeue()
            //is falling?
            let checkedSupports = supportedBy |> Map.find checkedBrick
            if checkedSupports |> Array.forall fallenBricks.Contains then
                //is falling
                fallenBricks.Add checkedBrick |> ignore
                match supporting |> Map.tryFind checkedBrick with
                | None -> ()
                | Some s -> s |> Array.iter bricksToCheck.Enqueue
                
        
        //brick, fallenBricks.Count - 1 //do not count the brick itself
        fallenBricks.Count - 1 //do not count the brick itself
        )
    |> Seq.sum
    |> printfn "PART 2: %A"

solve "inputs/testData22"    
solve "inputs/input22"    