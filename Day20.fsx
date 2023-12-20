#time

open System

open System.Collections.Generic
open System.IO


type ModuleType =
    | FlipFlop
    | Conjunction
    | Broadcast
    
type Pulse =
    | HighPulse
    | LowPulse
    
type ModuleState =
    | BroadcastState
    | FlipflopState of bool //on/off
    | ConjunctionState of Map<string, Pulse> //input module
    | ConjunctionNotInitializedState //not initialized conjunction module
    
type Module = {
    Label: string
    Type: ModuleType
    State: ModuleState
    TargetModules: string array
}
   

let parseModule (row: string) =
    let parts = row.Split("->", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

    let moduleType =
        match parts[0][0] with
        | '%' -> FlipFlop
        | '&' -> Conjunction
        | _ -> Broadcast
    
    
    {
        Label =
            match moduleType with
            | FlipFlop | Conjunction -> parts[0][1..]
            | _ -> parts[0]
        Type = moduleType
        State =
            match moduleType with
            | FlipFlop -> FlipflopState false
            | Broadcast -> BroadcastState
            | Conjunction -> ConjunctionNotInitializedState
        TargetModules = parts[1].Split(',', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    }   
   
let handlePulse (modul: Module) source (pulse: Pulse) =
   match modul.Type, pulse with
   | Broadcast, _ ->
       modul, modul.TargetModules |> Array.map (fun target -> target, pulse)
   | FlipFlop, HighPulse -> modul, [|  |]
   | FlipFlop, LowPulse ->
       match modul.State with
       | FlipflopState false ->
           { modul with State = FlipflopState true }, modul.TargetModules |> Array.map (fun target -> target, HighPulse)
       | FlipflopState true ->
           { modul with State = FlipflopState false }, modul.TargetModules |> Array.map (fun target -> target, LowPulse)
       | _ -> failwithf "Unexpected branch 1"
    | Conjunction, _ ->
       match modul.State with
       | ConjunctionState map ->
           let newState = map |> Map.add source pulse
           let pulseToSend = if newState |> Map.forall (fun _ p -> p = HighPulse) then LowPulse else HighPulse
           { modul with State = ConjunctionState newState }, modul.TargetModules |> Array.map (fun target -> target, pulseToSend)
       | _ -> failwithf "Unexpected branch 2"
   
   
let pushButton (modules: Map<string, Module>) =
    
    //breadth first search
    let mutable lowPulsesSent = 0L
    let mutable highPulsesSent = 0L
    
    // src * target * pulse
    let pulseQueue = Queue<string * string * Pulse>()
    pulseQueue.Enqueue("", "broadcaster", LowPulse)
    
    let modulesDict = Dictionary<string, Module>()
    modules |> Seq.iter (fun kv -> modulesDict.Add(kv.Key, kv.Value))
    
    while pulseQueue.Count > 0 do
        let src, target, p = pulseQueue.Dequeue()
        //printfn $"PULSE: %s{src} -%A{p}-> %s{target}"
        match p with
        | HighPulse -> highPulsesSent <- highPulsesSent + 1L
        | LowPulse -> lowPulsesSent <- lowPulsesSent + 1L

        if modulesDict.ContainsKey target then
            let targetModule = modulesDict[target]
            let updatedModule, nextTargets = handlePulse targetModule src p
            // if target = "a" then
            //     printfn $"ORIGIN A: %A{targetModule}"
            //     printfn $"UPDATED A: %A{updatedModule}"
            modulesDict[target] <- updatedModule
            nextTargets |> Array.iter (fun (next, pulse) -> pulseQueue.Enqueue(target, next, pulse))
    
    modulesDict |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq,
    lowPulsesSent,
    highPulsesSent
        
    
    
   
let solve1 (filename: string) =
    
    let file = File.OpenRead filename
    let reader = new StreamReader(file)
    
    let modules =
        let moduleMap = 
            seq {
                while reader.EndOfStream |> not do
                    yield reader.ReadLine() |> parseModule
            }
            |> Seq.map (fun x -> x.Label, x)
            |> Map.ofSeq
        
        let moduleBySources =
            moduleMap.Values
            |> Seq.collect (fun x -> x.TargetModules |> Array.map (fun y -> y, x.Label))
            |> Seq.groupBy fst
            |> Seq.map (fun (key, values) -> key, values |> Seq.map snd |> Array.ofSeq)
            |> Map.ofSeq
        
        moduleMap
        |> Map.map (fun _ v ->
            match v.State with
            | ConjunctionNotInitializedState ->
                { v with State =
                            moduleBySources
                            |> Map.find v.Label
                            |> Array.map (fun src -> src, LowPulse)
                            |> Map.ofArray
                            |> ConjunctionState }
            | _ -> v 
            )
    //modules
    
    let finalModules, l, h =
        seq { 1..1000 }
        |> Seq.fold (fun (m, lowCount, highCount) _ ->
            let updatedModules, addLow, addHigh = pushButton m
            updatedModules, addLow + lowCount, addHigh + highCount
            ) (modules, 0L, 0L)
    l * h

let r1 = solve1 "inputs/testData20a" // 32000000L
let r2 = solve1 "inputs/testData20b" // 11687500L
let r3 = solve1 "inputs/input20"
//
// let b = r1 |> Map.find "broadcaster"
// let a = r1 |> Map.find "a"
//
// handlePulse b "button" HighPulse
// handlePulse a "broadcaster" LowPulse
//
// pushButton r1
// pushButton r2
//
// let a2, p2 = handlePulse a "broadcaster" LowPulse
// let a3, p3 = handlePulse a2 "inv" LowPulse