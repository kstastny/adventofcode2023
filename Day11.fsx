#time

open System
open System.IO


let file = File.OpenRead("inputs/input11")
//let file = File.OpenRead("inputs/testData11")

let reader = new StreamReader(file)

let inputGrid =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine().ToCharArray()
    }
    |> Array.ofSeq


let emptyColumns =
     [|0..inputGrid[0].Length - 1|]
     |> Array.choose (fun j ->
         if [|0..inputGrid.Length - 1|]
            |> Array.map (fun i -> inputGrid[i][j])
            |> Array.forall (fun x -> x = '.')
            then
                Some (int64 j)
         else
             None
         )
     
let emptyRows =
     [|0..inputGrid.Length - 1|]
     |> Array.choose (fun i ->
         if [|0..inputGrid[0].Length - 1|]
            |> Array.map (fun j -> inputGrid[i][j])
            |> Array.forall (fun x -> x = '.')
            then
                Some (int64 i)
         else
             None
         )    

type Star = {X: int64; Y: int64}


let stars =
    seq { 
         for i in [0 .. (inputGrid.Length - 1)] do
            for j in  [0..inputGrid[0].Length - 1] do
                if inputGrid[i][j] = '#' then
                    yield { X = i; Y = j }
    }
    |> Array.ofSeq
    
 
let distance expansionFactor (star1: Star) (star2: Star) =
    Math.Abs(star1.X - star2.X)
    + Math.Abs(star1.Y - star2.Y)
    + (expansionFactor-1L) * (emptyRows |> Array.where (fun j -> j > star1.X && j < star2.X || j < star1.X && j > star2.X)
       |> Array.length |> int64)
    + (expansionFactor-1L) * (emptyColumns |> Array.where (fun i -> i > star1.Y && i < star2.Y || i < star1.Y && i > star2.Y)
       |> Array.length |> int64)
    
    
let distanceSum expansionFactor =
    seq {
        for i in [0..stars.Length - 1] do
            for j in [0..stars.Length - 1] do
                if i <> j then
                    yield distance expansionFactor stars[i] stars[j]
    }
    |> Seq.sum
    |> (fun x -> x / 2L)
    
printfn $"PART 1: %A{distanceSum 2L}"    
printfn $"PART 2: %A{distanceSum 1000000L}"    
