#time

open System

open System.Collections.Generic
open System.IO
open System.Diagnostics


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
   
let inline handlePulse (modul: Module) source (pulse: Pulse) =
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
    let mutable lowPulsesToRx = 0
    
    // src * target * pulse
    let pulseQueue = Queue<string * string * Pulse>()
    pulseQueue.Enqueue("", "broadcaster", LowPulse)
    
    let modulesDict = Dictionary<string, Module>()
    modules |> Seq.iter (fun kv -> modulesDict.Add(kv.Key, kv.Value))
    
    while pulseQueue.Count > 0 do
        let src, target, p = pulseQueue.Dequeue()
        // if src = "cs" || target = "cs" then
        //     printfn $"PULSE: %s{src} -%A{p}-> %s{target}"
        match p with
        | HighPulse -> highPulsesSent <- highPulsesSent + 1L
        | LowPulse -> lowPulsesSent <- lowPulsesSent + 1L
        
        if target = "rx" && p = LowPulse then lowPulsesToRx <- lowPulsesToRx + 1

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
    highPulsesSent,
    lowPulsesToRx
    
let pulseQueue = Queue<string * string * Pulse>()

let inline pushButton2 (modules: Dictionary<string, Module>) i =
    
    // src * target * pulse
    pulseQueue.Enqueue("", "broadcaster", LowPulse)
    
    while pulseQueue.Count > 0 do
        let src, target, p = pulseQueue.Dequeue()
        
        match modules.TryGetValue target with
        | true, targetModule ->
            let updatedModule, nextTargets = handlePulse targetModule src p
            //hack
//             
// Module "tg" emits high pulse to 'cs' in iteration 3769
// Module "kh" emits high pulse to 'cs' in iteration 3889
// Module "lz" emits high pulse to 'cs' in iteration 3917
// Module "hn" emits high pulse to 'cs' in iteration 4013
            
            if (nextTargets |> Array.tryHead = Some ("cs", HighPulse)) then
                printfn $"Module %A{updatedModule.Label} emits high pulse to 'cs' in iteration %i{i}"
            modules[target] <- updatedModule
            nextTargets |> Array.iter (fun (next, pulse) -> pulseQueue.Enqueue(target, next, pulse))
        | _ -> ()
    
    modules
        
    
let readModules (filename: string) =

    let file = File.OpenRead filename
    let reader = new StreamReader(file)

    let moduleMap = 
        seq {
            while reader.EndOfStream |> not do
                yield reader.ReadLine() |> parseModule
        }
        |> Seq.map (fun x -> x.Label, x)
        |> Map.ofSeq
    
    reader.Close()
    
    
    let sourcesByTarget =
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
                        sourcesByTarget
                        |> Map.find v.Label
                        |> Array.map (fun src -> src, LowPulse)
                        |> Map.ofArray
                        |> ConjunctionState }
        | _ -> v 
        )
        
    
   
let solve1 (filename: string) =
    
    let modules = readModules filename
    
    let finalModules, l, h =
        seq { 1..1000 }
        |> Seq.fold (fun (m, lowCount, highCount) _ ->
            let updatedModules, addLow, addHigh, lowToRx = pushButton m
            updatedModules, addLow + lowCount, addHigh + highCount
            ) (modules, 0L, 0L)
    l * h
//
// let r1 = solve1 "inputs/testData20a" // 32000000L
// let r2 = solve1 "inputs/testData20b" // 11687500L
// let r3 = solve1 "inputs/input20"

let printRev target (modules: Map<string, Module>) =

    let w = new StreamWriter(File.OpenWrite("day20-schema.txt"))
    let printed = HashSet<string>()

    let sourcesByTarget =
        modules.Values
        |> Seq.collect (fun x -> x.TargetModules |> Array.map (fun y -> y, x.Label))
        |> Seq.groupBy fst
        |> Seq.map (fun (key, values) -> key, values |> Seq.map snd |> Array.ofSeq)
        |> Map.ofSeq
        
        
    let rec loop i x =
        printfn $"loop %i{i}"
        if x |> List.isEmpty |> not then 
            w.WriteLine($"--------- LAYER %i{i} ------------")
            x
            |> List.collect (fun m ->
                if printed.Contains m then []
                else
                    printed.Add m |> ignore
                    match modules |> Map.tryFind m with
                    | Some m when m.Type = FlipFlop -> w.Write("flip-")
                    | Some m when m.Type = Conjunction -> w.Write("con-")
                    | _ -> ()
                    w.Write(m)
                    w.Write($"\t:")
                    
                    let sources = sourcesByTarget |> Map.tryFind m
                    match sources with
                    | None ->
                        w.WriteLine "NO SOURCES"
                        []
                    | Some s ->
                        s |> Array.iter (fun s ->
                            w.Write("\t")
                            match modules |> Map.tryFind s with
                            | Some m when m.Type = FlipFlop -> w.Write("flip-")
                            | Some m when m.Type = Conjunction -> w.Write("con-")
                            | _ -> ()                
                            w.Write s)
                        w.WriteLine()
                        s |> List.ofArray
            )
            |> loop (i+1)
        
        
    loop 0 [ target ]
        
    w.Close()
    
let modules = readModules "inputs/input20"
// printRev "rx" modules

let printState target depth (modules: Map<string, Module>) =
    
    let printed = HashSet<string>()
    
    let sourcesByTarget =
        modules.Values
        |> Seq.collect (fun x -> x.TargetModules |> Array.map (fun y -> y, x.Label))
        |> Seq.groupBy fst
        |> Seq.map (fun (key, values) -> key, values |> Seq.map snd |> Array.ofSeq)
        |> Map.ofSeq
        
        
    let rec loop i x =
        if x |> List.isEmpty |> not then
            let flipsOn =
                x
                |> List.choose (fun m ->
                    match modules |> Map.tryFind m with
                    | Some m when m.State = FlipflopState true -> Some m.Label
                    | _ -> None)
                //|> List.length
            let charges =
                x
                |> List.choose (fun m ->
                    match modules |> Map.tryFind m with
                    | Some m when m.Type = Conjunction ->
                        match m.State with
                        | ConjunctionState s ->
                            (m.Label, s |> Map.values |> Seq.where (fun v -> v = HighPulse) |> Seq.length, s.Count) |> Some
                        | _ -> None
                    | _ -> None)
            printfn $"L%i{i}: flipsOn: %A{flipsOn}, charges: %A{charges}"
        
        x |> List.iter (fun y -> printed.Add y |> ignore)
        let sources =
            x
            |> List.choose (fun y -> sourcesByTarget |> Map.tryFind y |> Option.map List.ofArray)
            |> List.concat
            |> List.where (fun y -> printed.Contains y |> not)
            
        if i < depth then
            loop (i + 1) sources
        
        
    loop 0 [ target ]    


let getConjunctionCharges (m: Module) =
    match m.State with
    | ConjunctionState s ->
        (m.Label, s |> Map.values |> Seq.where (fun v -> v = HighPulse) |> Seq.length, s.Count)
        |> Some
    | _ -> None
    
let getFlipFlopValue (m: Module) =
    match m.State with
    | FlipflopState x -> Some x
    | _ -> None
                        
let printIfFull modules m i =
    match modules |> Map.find m |> getConjunctionCharges with
    | None -> ()
    | Some (label, charged, count) ->
        if charged = count then
            printfn $"%s{m} is FULL on iteration %i{i}"
            
let printIfFull2 (modules: Dictionary<string, Module>) m i =
    match modules.TryGetValue m with
    | true, cm ->
        match cm |> getConjunctionCharges with
        | None -> ()
        | Some (label, charged, count) ->
            if charged = count then
                printfn $"%s{m} is FULL on iteration %i{i}"            
    | _ -> ()
            
let printIfFull3 (m: Module) i =
    match m |> getConjunctionCharges with
    | None -> ()
    | Some (label, charged, count) ->
        if charged = count then
            printfn $"%s{m.Label} is FULL on iteration %i{i}"            
            

let printCharges modules m =
    match modules |> Map.find m |> getConjunctionCharges with
    | None -> ()
    | Some (label, charged, count) ->
        printf $" %s{m} charges: %i{charged}/%i{count}"

let printIfFlipped modules1 modules2 m i =
    match modules1 |> Map.find m |> getFlipFlopValue,
          modules2 |> Map.find m |> getFlipFlopValue with
    | Some x, Some y when x <> y ->
        printfn $"%s{m} flipped at %i{i}, currently is %b{y}"
    | _ -> ()
    


let solve2 (filename: string) =
    
    let modules = readModules filename
    
    
    printState "rx" 5 modules
    printfn "------"
    
    let stopwatch = new Stopwatch()
    stopwatch.Start()
    
    //1M takes 17961, 19161, 20703
    //1M without printIfFull checks 17010, 15656, 16026
    //1M pushButton2 without printIfFull 15599, 14935, 14221 15348
    //1M pushButton2, printIfFull2, dictionary 7488, 7003, 6733, 6671
    //1M pushButton2, printIfFull3, dictionary 7091, 6853, 5975, 6009
    //1M pushButton2, task 7440, 7387, 6424
    //1M with inline 7081, 6740, 6824
 //   let a = Int32.MaxValue / 1000000 * 7 //15029
    //15029 / 3600
    
    let modulesDict = Dictionary<string, Module>()
    modules |> Seq.iter (fun kv -> modulesDict.Add(kv.Key, kv.Value))
    
    let zvModule = modulesDict["zv"]
    let skModule = modulesDict["sk"]
    let sdModule = modulesDict["sd"]
    let plModule = modulesDict["pl"]
    
    let finalModules =
        seq { 1..Int32.MaxValue }
        //seq { 1..20 }
        |> Seq.fold (fun m i ->
            
            let updatedModules = pushButton2 m i
            
           // printState "rx" 3 (updatedModules|> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq)
            
            //printfn $"%i{i}: lowToRx = %i{lowToRx}"
            if i % 1000000 = 0 then
                printf $"iteration %i{i / 1000000}M, took %A{stopwatch.ElapsedMilliseconds}ms to calculate"
                
                let modulesMap = updatedModules |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
                printCharges modulesMap "zv"
                printCharges modulesMap "sk"
                printCharges modulesMap "sd"
                printCharges modulesMap "pl"
                printfn ""
                stopwatch.Restart()
            
            // printState "rx" 5 updatedModules
            // // printfn "------"
            // printIfFull2 updatedModules "zv" i
            // printIfFull2 updatedModules "sk" i
            // printIfFull2 updatedModules "sd" i
            // printIfFull2 updatedModules "pl" i
            // printfn "------"
            
            //task {
                //printIfFull2 updatedModules "hn" i 
                printIfFull3 zvModule i            
                printIfFull3 skModule i            
                printIfFull3 sdModule i            
                printIfFull3 plModule i            
              //  ()
            //} |> ignore
            
            // printIfFull updatedModules "hn" i
            // printIfFull updatedModules "kh" i
            // printIfFull updatedModules "lz" i
            // printIfFull updatedModules "tg" i
            //
            //printIfFlipped m updatedModules "cm" i // period 8
            //printIfFlipped m updatedModules "db" i // not first level periodical 
            
            updatedModules
            ) modulesDict
    finalModules
    0

let r4 = solve2 "inputs/input20" //

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