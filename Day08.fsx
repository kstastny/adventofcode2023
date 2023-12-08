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


// PART 1
//TODO without mutable
// let mutable node = (nodeMap |> Map.find "AAA")
// let mutable stepIndex = 0
// while node.Label <> "ZZZ" do
//      let direction = path[stepIndex % path.Length]
//      //printfn $"stepIndex = %i{stepIndex}, node = %A{node}, direction = %A{direction}"
//      
//      node <-
//          match direction with
//          | 'L' -> nodeMap |> Map.find node.Left
//          | 'R' -> nodeMap |> Map.find node.Right
//          | _ -> failwith "wrong direction"
//      stepIndex <- stepIndex + 1
//          
// printfn $"Steps taken: %i{stepIndex + 1}"         


//
// Seq.unfold (fun (node, stepIndex) ->
//     let direction = path[stepIndex % path.Length]
//     
//     let nextNode =
//         match direction with
//         | 'L' -> nodeMap |> Map.find node.Left
//         | 'R' -> nodeMap |> Map.find node.Right
//     
//     Some (nextNode, stepIndex + 1)    
//     )
//     ((nodeMap |> Map.find "AAA") 0)

// seq { 0..Int32.MaxValue }
// |> Seq.mapf (fun node stepIndex ->
//     let direction = path[stepIndex % path.Length]
//     
//     let nextNode =
//         match direction with
//         | 'L' -> nodeMap |> Map.find node.Left
//         | 'R' -> nodeMap |> Map.find node.Right
//     
//     nextNode        
//     )
//  
//  (nodeMap |> Map.find "AAA")


//PART 2
let mutable nodes = (nodeMap |> Map.values) |> Array.ofSeq |> Array.where (fun x -> x.Label.EndsWith "A")

printfn $"nodes = %A{nodes |> Array.map _.Label}"
//
// let visitedSets = HashSet<string>()
// visitedSets.Add(nodes |> Array.map _.Label |> (fun x -> String.Join(",", x)))

let nextNode (n: Node) direction =
    match direction with
     | 'L' -> nodeMap |> Map.find n.Left
     | 'R' -> nodeMap |> Map.find n.Right
     | _ -> failwith "wrong direction"
     
     
let isTargetNode (n: Node) = n.Label.EndsWith "Z"     
     
//TODO
let indices =
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
    
printfn "path length: %i" path.Length

indices[0] |> Array.pairwise
|> Array.map (fun (x,y) -> y - x)

let differences =
    indices
    |> Array.map (fun x ->
        x
        |> Array.map (fun x -> x + 1 )
        |> Array.pairwise
        |> Array.map (fun (x,y) -> y - x)
        |> Array.head
    )

indices[0][0]    
indices[0][1]    
indices[0][2]    
    
    
differences
|> Array.map int64
|> Array.fold (*) 1L

let sets = 
    indices
    |> Array.map Set.ofArray

let factors = 

// 1087727835
let it: int = 1222348107

float it / float differences[0]

let aaa = 20513 * 12083 * 14893 * 19951 * 22199 * 17141

float aaa / float differences[0]


let xxx = 73L * 43L * 53L * 71L * 79L * 61L * 281L //result


differences|> Array.map (fun x -> xxx % int64 x = 0L)


(*
All the factors of 20513 :
1, 73, 281, 20513

All the factors of 12083 :
1, 43, 281, 12083

All the factors of 14893 :
1, 53, 281, 14893

All the factors of 19951 :
1, 71, 281, 19951

All the factors of 22199 :
1, 79, 281, 22199

All the factors of 17141 :
1, 61, 281, 17141
*)


    
    
    
sets |> Array.map (fun x -> x |> Set.maxElement) |> Array.max   

// sets[1..]
// |> Array.fold Set.intersect sets[0]

Set.intersectMany sets

let mutable stepIndex = 0
while nodes |> Array.exists (fun x -> x.Label.EndsWith "Z" |> not) do
     let direction = path[stepIndex % path.Length]
     //printfn $"stepIndex = %i{stepIndex}, nodes = %A{nodes |> Array.map _.Label}, direction = %A{direction}"
     
     if stepIndex % 100000 = 0 then
        printfn $"stepIndex = %i{stepIndex}, nodes = %A{nodes |> Array.map _.Label}, direction = %A{direction}"
     
     
     nodes <-
         nodes
         |> Array.map (fun n ->
            match direction with
             | 'L' -> nodeMap |> Map.find n.Left
             | 'R' -> nodeMap |> Map.find n.Right
             | _ -> failwith "wrong direction"             
             )
         
     stepIndex <- stepIndex + 1
         
printfn $"Steps taken: %i{stepIndex + 1}"    