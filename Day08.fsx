#time

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions


let file = File.OpenRead("inputs/input08")


// let file = File.OpenRead("inputs/testData08")
// let file = File.OpenRead("inputs/testData08b")


//let file = File.OpenRead("inputs/testData08c")
let reader = new StreamReader(file)

let inputArray =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Array.ofSeq
    

type Node = {
    Label: string
    Right: string
    Left: string
}

let parseNode (x: string) =    
    {
        Label = x[0..2]
        Right = x[12..14]
        Left = x[7..9]
    }
    
let nodeMap =
    inputArray[2..]
    |> Array.map parseNode
    |> Array.map (fun x -> x.Label, x)
    |> Map.ofArray
        
let path = inputArray[0].ToCharArray()

let nextNode (n: Node) direction =
    match direction with
     | 'L' -> nodeMap |> Map.find n.Left
     | 'R' -> nodeMap |> Map.find n.Right
     | _ -> failwith "wrong direction"


// PART 1
//TODO without mutable
let mutable node = (nodeMap |> Map.find "AAA")
let mutable stepIndex = 0
while node.Label <> "ZZZ" do
     let direction = path[stepIndex % path.Length]
     //printfn $"stepIndex = %i{stepIndex}, node = %A{node}, direction = %A{direction}"
     
     node <-  nextNode node direction
     stepIndex <- stepIndex + 1
         
printfn $"PART 1: %i{stepIndex}"         


//PART 2
let nodes = (nodeMap |> Map.values) |> Array.ofSeq |> Array.where (fun x -> x.Label.EndsWith "A")

// printfn $"nodes = %A{nodes |> Array.map _.Label}"
     
let isTargetNode (n: Node) = n.Label.EndsWith "Z"     
     

//TODO without mutable
let endNodeIndices =
    nodes
    |> Array.Parallel.map (fun startNode ->
        let mutable node = startNode
        let mutable stepIndex = 0
        seq {
            while true do
                let direction = path[stepIndex % path.Length]
                
                node <- nextNode node direction
                stepIndex <- stepIndex + 1
                
                if node |> isTargetNode then
                    yield stepIndex
        }
        |> Seq.take 2
        |> Array.ofSeq
        )

//how many steps it takes to reach next targetNode    
let differences =
    endNodeIndices
    |> Array.map (fun x ->
        x
        |> Array.map (fun x -> x + 1 )
        |> Array.pairwise
        |> Array.map (fun (x,y) -> y - x)
        |> Array.head
        |> int64
    )

let factors (n: int64) =
    seq {
        for x in seq { 1L..(n/2L) } do
            if n % x = 0 then
                yield x
        yield n
    }
    
(*
Solving PART 2
 - for each start node, find regularity in reaching end nodes (turns out there are loops so the differences are always the same)
 - find out when all the periods will match - smallest number divisible by all periods
    - do this by finding out all factors and multiplying the unique factors
    - in reality we are finding least common multiple, there are better approaches (see wiki https://en.wikipedia.org/wiki/Least_common_multiple)
*)    
    
let uniqueFactors =
    differences
    |> Array.collect (fun x ->
        //skip the last factor, we just wants what is composed from
        let f = x |> factors |> Array.ofSeq
        f[1..f.Length - 2]
        )
    |> Array.distinct
    
printfn $"PART 2: %A{uniqueFactors |> Array.fold (*) 1L}"
