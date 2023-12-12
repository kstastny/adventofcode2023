#time

open System
open System.Text
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let file = File.OpenRead("inputs/input12")
//let file = File.OpenRead("inputs/testData12")

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
            m.Value |> Int64.Parse |> Some
        else
            None)
    |> Array.ofSeq   
    
let parse (row: string) =
    let conditions = row.Split(' ')[0]
    let groups = row.Split(' ')[1]
    conditions, (groups |> getNumbers)
    
// let parse (row: string) =
//     let conditions = row.Split(' ')[0]
//     let groups = row.Split(' ')[1]
//     //conditions, (groups |> getNumbers)
//     //unfold TODO generic
//     $"{conditions}?{conditions}?{conditions}?{conditions}?{conditions}",
//     $"{groups},{groups},{groups},{groups},{groups}" |> getNumbers
    
    
        
    
        
    
let permutations (conditions: string) =
    let rec loop li =
        match li with
        | [] -> []
        | [ head ] ->
            if head = '?' then
                [
                    [ '.' ]
                    [ '#' ]
                ]
            else
                [ [ head]  ]
        | head::tail when head = '?' ->
            loop tail
            |> List.collect (fun x ->
                [
                  '.'::x
                  '#'::x
                ]
                )
            
        | head::tail ->
                loop tail
                |> List.map (fun x -> head::x)            
            
        
    conditions.ToCharArray()
    |> List.ofArray
    |> loop
    |> List.map (fun x -> x |> Array.ofList |> String)
    |> List.distinct


let arrangements (row: string) =
    let conditions, groups = parse row
    let perm = permutations conditions
    
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
    
inputArray
|> Array.map arrangements
|> Array.sum

// // inputArray[1] |> arrangements
// inputArray[1] |> parse //".??..??...?##.?.??..??...?##.?.??..??...?##.?.??..??...?##.?.??..??...?##." - 8x8x8x8x4
//
//
// arrangements inputArray[0]
//
// // inputArray[4]
// // permutations inputArray[4]
// arrangements inputArray[4]
// // arrangements inputArray[5]