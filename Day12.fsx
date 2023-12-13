#time

open System
open System.Diagnostics
open System.Text
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Threading

//let file = File.OpenRead("inputs/input12")
let file = File.OpenRead("inputs/testData12")

let reader = new StreamReader(file)

let inputArray =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Array.ofSeq
    
    
let numRegex = Regex("[-]?[0-9]*")

let getNumbers (x: string) =
    numRegex.Matches(x)
    |> Seq.choose (fun m ->
        if String.IsNullOrWhiteSpace m.Value |> not then
            m.Value |> Int32.Parse |> Some
        else
            None)
    |> Array.ofSeq   
    
let parse (row: string) =
    let conditions = row.Split(' ')[0]
    let groups = row.Split(' ')[1]
    conditions, (groups |> getNumbers)
    
    
        
let unfold2  (row: string) =
    let conditions, groups = parse row
    $"{conditions}?{conditions}",
    [|0..1|] |> Array.collect (fun _ -> groups)
    
        
    
let permutations (conditions: string) (groups: int array) =
    
    let groupSum = groups |> Array.sum
    
    let condList =
        conditions.ToCharArray() |> List.ofArray
    
    let questionMarks = condList |> List.where (fun x -> x = '?') |> List.length
    let generatedHashCount =
        let currentHashes =
            condList |> List.where (fun x -> x = '#') |> List.length
        groupSum - currentHashes
    //choose generatedHashCount positions from questionMarks    
    //printfn $"Calculating permutations for {questionMarks} over {generatedHashCount}"
    //let maxGroup = groups |> Array.max
    
    //NOTE: consecutive hashes BEFORE inserting this new character. groupIndex is for the NEW group, yet not built
    let canInsertDot (consecutiveHashes: int) groupIndex justCompletedGroup =
        justCompletedGroup
        || consecutiveHashes = 0
        //|| groups |> Array.exists (fun i -> i = consecutiveHashes) //I do not want this - this would match any group!
        || groupIndex >= groups.Length //all groups done
       // || consecutiveHashes >= groups[groupIndex] //building group TODo this is wrong
        //|| consecutiveHashes > 0 && consecutiveHashes //building group because justCompletedGroup is false
        
    let canInsertHash (consecutiveHashes: int) groupIndex justCompletedGroup =
        justCompletedGroup |> not &&
        groupIndex < groups.Length && consecutiveHashes < groups[groupIndex] //we are building new group specified by groupIndex
    
    //n - total number to select; R - count of hashes we are selecting
    let rec loop n r li consecutiveHashes groupIndex justCompletedGroup =
        
        //printfn $"perm = %A{li}"
        
        //inserting hash completes group, for new hashes we increase groupIndex
        let hashCompletesGroup =
            groupIndex < groups.Length && consecutiveHashes+1 = groups[groupIndex]
        
        let expanded =
            match li with
            | [] -> []
            | [ head ] ->
                if head = '?' then
                    [
                        if r = 0 then
                            [ '.' ]
                        if r = n then //should be 1 in this case
                            [ '#' ]
                    ]
                else
                    [ [ head]  ]
            | head::tail when head = '?' ->
                if r = 0 then
                    if canInsertDot consecutiveHashes groupIndex justCompletedGroup then
                        loop (n - 1) 0 tail 0 groupIndex false 
                        |> List.map (fun t -> '.'::t)
                    else
                        []
                elif r = n then
                    if canInsertHash consecutiveHashes groupIndex justCompletedGroup then
                        let newGroupIndex = //TODO can be used every time hash is inserted
                            if hashCompletesGroup then groupIndex + 1 else groupIndex
                        loop (n - 1) (r - 1) tail (consecutiveHashes + 1) newGroupIndex hashCompletesGroup 
                        |> List.map (fun t -> '#'::t)
                    else
                        []
                else
                    let firstDot =
                        if canInsertDot consecutiveHashes groupIndex justCompletedGroup then
                            loop (n - 1) r tail 0 groupIndex false
                            |> List.map (fun t -> '.'::t)
                        else
                            []
                    let firstHash =
                        if canInsertHash consecutiveHashes groupIndex justCompletedGroup then
                            let newGroupIndex =
                                if hashCompletesGroup then groupIndex + 1 else groupIndex
                            loop (n - 1) (r - 1) tail (consecutiveHashes + 1) newGroupIndex hashCompletesGroup 
                            |> List.map (fun t -> '#'::t)
                        else
                            []
                    firstDot @ firstHash
            | '#'::tail ->
                    if canInsertHash consecutiveHashes groupIndex justCompletedGroup then
                        let newGroupIndex =
                            if hashCompletesGroup then groupIndex + 1 else groupIndex
                        loop n r tail (consecutiveHashes + 1) newGroupIndex hashCompletesGroup
                        |> List.map (fun x -> '#'::x)
                    else
                        []
            | '.'::tail ->
                    if canInsertDot consecutiveHashes groupIndex justCompletedGroup then
                        loop n r tail 0 groupIndex false
                        |> List.map (fun x -> '.'::x)
                    else
                        []                    
                
            | head::tail ->
                    //NOTE: this should not happen
                    printfn $"SOMETHING WEIRD, unexpected character: %c{head}"
                    let hashes = 0
                    loop n r tail hashes groupIndex false
                    |> List.map (fun x -> head::x)
        // printfn $"perm = %A{li}"                    
        // printfn $"expanded = %A{expanded}"
        expanded
        
            
    loop questionMarks generatedHashCount condList 0 0 false
    |> List.map (fun x -> x |> Array.ofList |> String)
    //|> List.distinct
//
let conditions, groups = parse "???.### 1,1,3"
// let conditions2, groups2 = unfold2 "?#?#?#?#?#?#?#? 1,3,1,6"
// permutations conditions2 groups2


let arrangements (row: string) =
    let conditions, groups = parse row
    //let groupSum = groups |> Array.sum |> int32
    let perm = permutations conditions groups
    
    let expCount = groups |> Array.sum
    
    let regex : Regex =
        let sb = StringBuilder()
        for n in groups do
            //TODO first has to be start of string or
            sb.Append("(#){nnn}(\.)+".Replace("nnn", string n)) |> ignore
        //sb.Remove(sb.Length - 5, 5) |> ignore
        //let s = sb.ToString()
        let s = "(\.)+" + sb.ToString()
        //printfn $"regexString = %s{s}"
        Regex(s)
        
    perm
    |> Seq.where (fun x ->
        (x.ToCharArray() |> Array.where (fun y -> y = '#') |> Array.length |> int64 = expCount) &&
        regex.IsMatch("." + x + ".")
        )
    |> Seq.toArray
    |> Seq.length
    
let arrangementsInvalid (row: string) =
    let conditions, groups = parse row
    //let groupSum = groups |> Array.sum |> int32
    let perm = permutations conditions groups
    
    let expCount = groups |> Array.sum
    
    let regex : Regex =
        let sb = StringBuilder()
        for n in groups do
            //TODO first has to be start of string or
            sb.Append("(#){nnn}(\.)+".Replace("nnn", string n)) |> ignore
        //sb.Remove(sb.Length - 5, 5) |> ignore
        //let s = sb.ToString()
        let s = "(\.)+" + sb.ToString()
        //printfn $"regexString = %s{s}"
        Regex(s)
        
    perm
    |> Seq.where (fun x ->
        (x.ToCharArray() |> Array.where (fun y -> y = '#') |> Array.length |> int64 = expCount) &&
        regex.IsMatch("." + x + ".") |> not
        )
    |> Seq.toArray
    
    
let arrangements2 (conditions: string, groups) =
    
    let perm = permutations conditions groups
    
    let expCount = groups |> Array.sum
    
    let regex : Regex =
        let sb = StringBuilder()
        for n in groups do
            //TODO first has to be start of string or
            sb.Append("(#){nnn}(\.)+".Replace("nnn", string n)) |> ignore
        //sb.Remove(sb.Length - 5, 5) |> ignore
        //let s = sb.ToString()
        let s = "(\.)+" + sb.ToString()
        //printfn $"regexString = %s{s}"
        Regex(s)
        
    perm
    |> Seq.where (fun x ->
        //(x.ToCharArray() |> Array.where (fun y -> y = '#') |> Array.length |> int64 = expCount) &&
        regex.IsMatch("." + x + "."))
    |> Seq.toArray
    |> Seq.length
    |> int64
    

// let conditions2, groups2 = unfold2 inputArray[1]
// permutations conditions2 groups2    
// let connected = inputArray[1] |> unfold2 |> arrangements2    
    
inputArray
|> Array.map arrangements
|> Array.sum //7260

(*

    ???.### 1,1,3 - 1 arrangement
    .??..??...?##. 1,1,3 - 16384 arrangements
    ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
    ????.#...#... 4,1,1 - 16 arrangements
    ????.######..#####. 1,6,5 - 2500 arrangements
    ?###???????? 3,2,1 - 506250 arrangements

*)

// let regex : Regex =
//     let sb = StringBuilder()
//     for n in groups do
//         //TODO first has to be start of string or
//         sb.Append("(#){nnn}(\.)+".Replace("nnn", string n)) |> ignore
//     //sb.Remove(sb.Length - 5, 5) |> ignore
//     //let s = sb.ToString()
//     let s = "(\.)+" + sb.ToString()
//     //printfn $"regexString = %s{s}"
//     Regex(s)

//regex (#|\?){1}(\.|\?)+?(#|\?){1}(\.|\?)+?(#|\?){3}(\.|\?)+? - smallest matches
//TODO each match - get arrangements, then multiply all arrangements
  //TODO or somehow differently split into parts that can be evaluated separately
//
// //regex (#|\?){1}(\.|\?)+(#|\?){1}(\.|\?)+(#|\?){3}(\.|\?)+
// inputArray[0] |> unfold //"???.###????.###????.###????.###????.###" - 1,1,3 1x
// inputArray[1] |> unfold //".??..??...?##.?.??..??...?##.?.??..??...?##.?.??..??...?##.?.??..??...?##." - 1,1,3  4x8x8x8x8
// inputArray[2] |> unfold // ?#?#?#?#?#?#?#???#?#?#?#?#?#?#???#?#?#?#?#?#?#???#?#?#?#?#?#?#???#?#?#?#?#?#?#? 1,3,1,6,... 
// //inputArray[3] |> unfold // ????.#...#...?????.#...#...?????.#...#...?????.#...#...?????.#...#... 4,1,1... 1x4x4x4x4
// inputArray[4] |> unfold // ????.######..#####.?????.######..#####.?????.######..#####.?????.######..#####.?????.######..#####. 1,6,5... 4*5*5*5*5
// inputArray[5] |> unfold // ?###??????????###??????????###??????????###??????????###???????? 3,2,1... 15*15*15*15*10
// arrangements "???????? 2,1"   //15     
// arrangements "??????? 2,1"   //15
//
// let groups = inputArray[1] |> parse |> snd
//
// inputArray[0] |> unfold1Start2 |> arrangements2
// inputArray[0] |> unfold1End2 |> arrangements2
// inputArray[0] |> unfold1StartEnd |> arrangements2
//
// inputArray[1] |> arrangements
// inputArray[1] |> unfold1Start |> arrangements2
// inputArray[1] |> unfold1Start2 |> arrangements2
// inputArray[1] |> unfold1End |> arrangements2
// inputArray[1] |> unfold1End2 |> arrangements2
// inputArray[1] |> unfold1StartEnd |> arrangements2
//
// inputArray[1] |> unfold2 |> arrangements2
//
// inputArray[1] |> unfold3 |> arrangements2
// inputArray[1] |> unfold |> arrangements2
//
// inputArray[1] |> arrangements
// inputArray[5] |> unfold2 |> arrangements2
// inputArray[1] |> unfold1Start2 
// inputArray[1] |> unfold1End2
// inputArray[1] |> unfold1End2 |> arrangements2

printfn $"inputArray.length = %A{inputArray.Length}"

//TODO ..#?????????#?#???? 1,13 still problematic. new algorithm for permutations will fix it


// let index = 89
// let c,g = inputArray[index] |> parse 
// permutations c g |> List.length
// inputArray[index] |> arrangements |> int64
//
// // 37 permutations for index 89
//
// inputArray[index] |> arrangementsInvalid |> Array.length


let c2,g2 = inputArray[index] |> unfold2
permutations c2 g2 |> List.length
inputArray[index] |> unfold2 |> arrangements2

let stopwatch = Stopwatch()
stopwatch.Start()
let mutable cnt = 0

inputArray
|> Array.mapi (fun i x ->
    
    printfn $"%A{i} started at %A{stopwatch.Elapsed}, cnt = %i{cnt}"
    let single = x |> arrangements |> int64
    let connected = x |> unfold2 |> arrangements2
    printfn $"%A{x}, single={single}, connected={connected}"

    
    let res =
        single * (Math.Pow(connected/single |> float, 4) |> int64)
    //printfn $"%i{i} = {res} (single = {single}, connected = {connected})"
    //Interlocked.Increment(ref cnt) |> ignore //TODO this does not work
    cnt <- cnt + 1
    res
    )
|> Array.sum

Int32.MaxValue
// 1209410872943L is too low

//inputArray1 full data
(*
12
22 start
18 end
33 startEnd
216, but takes Real: 00:05:35.451, CPU: 00:06:03.875, GC gen0: 243, gen1: 8, gen2: 5 to calculate
    216 = 12 * 18, i.e. END * SINGLE
    TODO co unfold tolik kolik je první group? nebo nějak spojit jen do první tečky a brát jen první z další groupy?
*)


// // inputArray[1] |> arrangements
//
//
// arrangements inputArray[0]
//
// // inputArray[4]
// // permutations inputArray[4]
// arrangements inputArray[4]
// // arrangements inputArray[5]