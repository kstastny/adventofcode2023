#time

open System
open System.Text
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

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
    
    
let unfold (row: string) =
    let conditions, groups = parse row
    $"{conditions}?{conditions}?{conditions}?{conditions}?{conditions}",
    [|0..4|] |> Array.collect (fun _ -> groups)
    
    
let unfold1Start  (row: string) =
    let conditions, groups = parse row
    $"{conditions}?",
    [|0..0|] |> Array.collect (fun _ -> groups)   
        
let unfold1End  (row: string) =
    let conditions, groups = parse row
    $"?{conditions}",
    [|0..0|] |> Array.collect (fun _ -> groups)
    
let unfoldValidOnEnd  (row: string) =
    let conditions, groups = parse row
    $"{conditions}#",
    groups
        
let unfoldValidOnStart  (row: string) =
    let conditions, groups = parse row
    $"#{conditions}",
    groups  
        
    
let unfold1Start2  (row: string) =
    let conditions, groups = parse row
    let extraGroup = groups[0]
    $"{conditions}#{conditions[0..int32 extraGroup]}",
    [| extraGroup |] |> Array.append groups 
    
        
let unfold1End2  (row: string) =
    let conditions, groups = parse row
    let extraGroup = groups |> Array.last
  //  printfn $"XXX {conditions[conditions.Length - int32 extraGroup..]}"
    $"{conditions[conditions.Length - int32 extraGroup..]}?{conditions}",
    groups |> Array.append [| extraGroup |]
        
let unfold1StartEnd  (row: string) =
    let conditions, groups = parse row
    $"?{conditions}?",
    [|0..0|] |> Array.collect (fun _ -> groups)   
        
let unfold2  (row: string) =
    let conditions, groups = parse row
    $"{conditions}?{conditions}",
    [|0..1|] |> Array.collect (fun _ -> groups)
    
let unfold3  (row: string) =
    let conditions, groups = parse row
    $"{conditions}?{conditions}?{conditions}",
    [|0..2|] |> Array.collect (fun _ -> groups)    
        
        
    
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
    
    let 
    
    
    //let rec loop n r li consecutiveHashes =
    let rec loop n r li =
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
                loop (n - 1) 0 tail 
                |> List.map (fun t -> '.'::t)
            elif r = n then
                loop (n - 1) (r - 1) tail 
                |> List.map (fun t -> '#'::t)
            else
                let firstDot =
                    loop (n - 1) r tail 
                    |> List.map (fun t -> '.'::t)
                let firstHash =
                    loop (n - 1) (r - 1) tail 
                    |> List.map (fun t -> '#'::t)
                firstDot @ firstHash
            
        | head::tail ->
                //let hashes = if head = '#' then consecutiveHashes + 1 else 0
                loop n r tail //hashes
                |> List.map (fun x -> head::x)            
            
        
    condList
    |> loop questionMarks generatedHashCount// 0
    |> List.map (fun x -> x |> Array.ofList |> String)
    //|> List.distinct
//
// let conditions, groups = parse "???.### 1,1,3"
// let sum = groups |> Array.sum
//
// permutations conditions (int32 sum)

// let rec permutations2 (li: char list) (n: int) (r: int) =
//     //all permutations of length with N hashes
//     if n = 0 then
//         [ li ]
//     elif r = 0 then
//         (permutations2 li (n - 1) 0)
//         |> List.map (fun t -> '.'::t)
//     elif r = n then
//         (permutations2 li (n - 1) (r - 1))
//         |> List.map (fun t -> '#'::t)    
//     else
//         let li1 =
//             (permutations2 li (n - 1) r)
//             |> List.map (fun t -> '.'::t)
//         
//         let li2 =
//             (permutations2 li (n - 1) (r - 1))
//             |> List.map (fun t -> '#'::t)
//         li1 @ li2


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
        regex.IsMatch("." + x + "."))
    |> Seq.toArray
    |> Seq.length
    
let arrangements2 (conditions: string, groups) =
    let groupSum = groups |> Array.sum |> int32
    let perm = permutations conditions groupSum
    
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
        regex.IsMatch("." + x + "."))
    |> Seq.toArray
    |> Seq.length
    |> int64   
    
// inputArray
// |> Array.map arrangements
// |> Array.sum

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
// inputArray
// |> Array.mapi (fun i x ->
//     
//     let singleStart2 = x |> unfold1Start2 |> arrangements2
//     let singleEnd2 = x |> unfold1End2 |> arrangements2
//     
//     //let double = x |> unfold2 |> arrangements2
//     
//     let conditions, groups = parse x
//     let validStart =
//         conditions[0] = '.' || conditions[conditions.Length - 1] = '.' ||
//         //x |> unfoldValidOnStart |> arrangements2 > 0
//         singleStart2 > 0
//     let validEnd =
//         conditions[0] = '.' || conditions[conditions.Length - 1] = '.' ||
//         //x |> unfoldValidOnEnd |> arrangements2 > 0
//         singleEnd2 > 0
//     //x, single, (singleStart, singleEnd), (singleStart2, singleEnd2), singleStartEnd//, double//, triple
//    // x, single, (singleStart, singleEnd), (validStart, validEnd), singleStartEnd//, double//, triple
//     
//     let res = 
//         if validStart && validEnd then
//             let singleStart = x |> unfold1Start |> arrangements2
//             let singleEnd = x |> unfold1End |> arrangements2
//             let singleStartEnd = x |> unfold1StartEnd |> arrangements2
//             //connecting multiplies options
//             singleStart * singleEnd * singleStartEnd * singleStartEnd * singleStartEnd
//         else
//             let single = x |> arrangements
//             Math.Pow(single, 5) |> int64
//     printfn $"%i{i} = {res}"            
//     res
//     
//     // if double = singleStart * singleEnd then
//     //     //connecting multiplies options
//     //     singleStart * singleEnd * singleStartEnd * singleStartEnd * singleStartEnd
//     // else
//     //     //TODO also calculating double is too slow
//     //     //connecting does not multiply options TODO then what?
//     //     printfn $"SUSP: %A{x}"
//     //     Math.Pow((x |> arrangements) |> float, 5) |> int32
//     )
// |> Array.sum
//gives 1392071027153L //TOO LOW;

//TODO ..#?????????#?#???? 1,13 still problematic. new algorithm for permutations will fix it

// inputArray
// |> Array.mapi (fun i x ->
//     
//     let singleStart = x |> unfold1Start |> arrangements2
//     let singleEnd = x |> unfold1End |> arrangements2
//     let singleStartEnd = x |> unfold1StartEnd |> arrangements2
//     
//     let double = x |> unfold2 |> arrangements2
//     
//     let res =     
//         if double = singleStart * singleEnd then
//             //connecting multiplies options
//             singleStart * singleEnd * singleStartEnd * singleStartEnd * singleStartEnd
//         else //TODO double gives us the ratio of two connected
//             //TODO also calculating double is too slow
//             //connecting does not multiply options TODO then what?
//             let single = (x |> arrangements) 
//             printfn $"SUSP: %A{x} (single = {single}, double = {double})"
//             Math.Pow(single |> float, 5) |> int64
//     printfn $"%i{i} = {res}"
//     res
//     )
// |> Array.sum

// let conditions, groups = inputArray[1] |> unfold2
// let groupSum = groups |> Array.sum |> int32
// //let perm = permutations conditions groupSum
// let condList =
//         conditions.ToCharArray() |> List.ofArray
//     
// let questionMarks = condList |> List.where (fun x -> x = '?') |> List.length
// let generatedHashCount =
//     let currentHashes =
//         condList |> List.where (fun x -> x = '#') |> List.length
//     groupSum - currentHashes


inputArray
|> Array.Parallel.mapi (fun i x ->
    
    let single = x |> arrangements |> int64
    let connected = x |> unfold2 |> arrangements2

    
    let res =
        single * (Math.Pow(connected/single |> float, 4) |> int64)
    printfn $"%i{i} = {res} (single = {single}, connected = {connected})"
    res
    )
|> Array.sum 

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