#time

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO

let file = File.OpenRead("inputs/input25")
//let file = File.OpenRead("inputs/testData25")

let reader = new StreamReader(file)



let inputRows =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Array.ofSeq
    
let inputConnections =
    inputRows
    |> Array.collect (fun row ->
        let parts = row.Split(":", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        parts[1]
            .Split(' ', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            |> Array.collect (fun x ->
                [|
                   parts[0], x
                   //x, parts[0]
                |])
        )
    

let groups (connections: (string * string) array) =
    
    //printfn $"connections %A{connections.Length}"
    
    let connectionsByNode =
        connections
        |> Array.collect (fun (x,y) -> [| x,y ; y,x |])
        |> Array.groupBy fst
        |> Map.ofArray
    
    //let nodes = connectionsByNode.Keys |> set
    let nodesToCheck = HashSet<string>()
    connectionsByNode.Keys |> Seq.iter (fun x -> nodesToCheck.Add x |> ignore)
    
    let rec search s n =
        let newNeighbors =
            connectionsByNode
            |> Map.find n
            |> Array.map snd
            |> Array.where (fun x -> s |> Set.contains x |> not)
        
        if newNeighbors.Length > 0 then
            newNeighbors
            |> Array.fold (fun xS n ->
                search xS n
                )
                (Set.union s (newNeighbors |> set))
        else
            s
            
    
    seq {
        while nodesToCheck.Count > 0 do
            let n = nodesToCheck |> Seq.head
            let groupNodes = search Set.empty n
            groupNodes |> Seq.iter (nodesToCheck.Remove >> ignore)
            yield groupNodes
    }
    
    
inputConnections.Length



type Edge = {
    Code: string //original code of edge
    Destination: string //code of destination node
}

type Node = {
    Code: string
    Edges: Edge list //outgoing edges, may even target the same node
}

type Graph = {
    Nodes: Map<string, Node>
}

let buildGraph (edges: (string * string) array ) =
    //edges are specified only in one direction
    let allDirections = edges |> Array.collect (fun (x,y) -> [| x,y ; y,x |])
    
    let nodes = 
        allDirections
        |> Array.groupBy fst
        |> Array.map (fun (src, destinations) ->
            src, {
                Code = src
                Edges = destinations
                        |> Array.map snd 
                        |> Array.map (fun d -> { Code = $"{src}-{d}" ; Destination = d }) |> List.ofArray
            })
        |> Map.ofArray
    {
        Nodes = nodes
    }
    
let g = buildGraph inputConnections

let rand = Random()

//https://www.geeksforgeeks.org/introduction-and-implementation-of-kargers-algorithm-for-minimum-cut/
let karger (g: Graph) =
    (*
    1)  Initialize contracted graph CG as copy of original graph
    2)  While there are more than 2 vertices.
          a) Pick a random edge (u, v) in the contracted graph.
          b) Merge (or contract) u and v into a single vertex (update 
             the contracted graph).
          c) Remove self-loops
    3) Return cut represented by two vertices.
    *)
    
    let mutable nodes = g.Nodes
    while nodes.Count > 2 do
        let randNode = nodes.Values |> Seq.skip (rand.Next(nodes.Count)) |> Seq.head
        let randEdge = randNode.Edges |> List.item (rand.Next(randNode.Edges |> List.length))
        let removedNode = nodes |> Map.find randEdge.Destination
        
        //contract nodes
        let contractedNode =
             { randNode with Edges = randNode.Edges @ removedNode.Edges }
        nodes <- nodes |> Map.remove removedNode.Code
        nodes <- nodes |> Map.add contractedNode.Code contractedNode
        nodes <- nodes |> Map.map (fun _ n ->
            if n.Code = contractedNode.Code then
                //remove self loops
                { n with Edges = n.Edges |> List.where (fun x -> x.Destination <> contractedNode.Code && x.Destination <> removedNode.Code) } 
            else
                // update references
                { n with Edges = n.Edges
                                 |> List.map (fun x ->
                                    if x.Destination = removedNode.Code then
                                        { x with Destination = contractedNode.Code }
                                    else
                                        x)
                }
            )
    nodes

Seq.init
    10
    (fun i ->
        printfn $"%A{i}"
        karger g)
|> Seq.map Seq.head //both nodes are just pointing towards each other
|> Seq.map (fun kv -> kv.Value)
|> Seq.minBy (fun n -> n.Edges |> List.length)
    
(*
  { Code = "dpx"
    Edges =
     [{ Code = "rmg-fql"
        Destination = "qql" }; { Code = "mfc-vph"
                                 Destination = "qql" };
      { Code = "vmt-sfm"
        Destination = "qql" }] }

*)    
    
// count of connections from each node     
// let connCounts =
//     inputConnections
//     |> Array.collect (fun (x,y) -> [| x,y ; y,x |])
//     |> Array.groupBy fst
//     |> Map.ofArray
//     |> Map.map (fun k v -> v |> Array.length)
//     |> Map.toSeq
//     |> Seq.sortBy snd
//     |> Array.ofSeq

    
let isConnection a b x y=
    (a = x && b = y) || (a = y && b = x)

let arr =     
    inputConnections
    |> Array.where (fun (x,y) ->
        not(isConnection "rmg" "fql" x y)
        && not(isConnection "mfc" "vph" x y)
        && not(isConnection "vmt" "sfm" x y)
        )
    |> groups
    |> Array.ofSeq
//     
arr.Length    
arr[0].Count * arr[1].Count