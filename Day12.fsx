#time

open System
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
            m.Value |> Int32.Parse |> Some
        else
            None)
    |> Array.ofSeq   
    
let parse (row: string) =
    let conditions = row.Split(' ')[0]
    let groups = row.Split(' ')[1]
    conditions, (groups |> getNumbers)
    
let unfold5  (row: string) =
    let conditions, groups = parse row
    $"{conditions}?{conditions}?{conditions}?{conditions}?{conditions}",
    [0..4] |> List.collect (fun _ -> groups |> List.ofArray)  
        
    
let permCountMemoized (conditions: string) (groups: int list) =
    let cache = System.Collections.Concurrent.ConcurrentDictionary<string * int list, int64>()
    
    let rec loop (conditions: string) (groups: int list) =
        cache.GetOrAdd(
               (conditions, groups),
               (fun (_,_) ->
                    //printfn $"permCount %s{conditions} %A{groups}"
                    match conditions, groups with
                    | "", _::_ -> 0L
                    | _, [] ->
                        // '.' and '?' are fine
                        if conditions.IndexOf('#') > -1 then 0L else 1L
                        
                    | _, head::tail ->
                        //latestPossibleStart should be sum of ints in groups + groups.Count - 1 for differences
                        let latestPossibleStart =
                            if conditions.IndexOf('#') > -1
                                then
                                    conditions.IndexOf('#')
                                else
                                conditions.Length - ((groups |> List.sum) + ((groups |> List.length) - 1))
                            
                        [0..latestPossibleStart]
                        |> List.where (fun i ->
                          //  printfn "i = %i, head = %i" i head
                            let followingIndex = head + i
                            conditions.Length >= followingIndex 
                            && (conditions.Length <= followingIndex || conditions[followingIndex] = '.' || conditions[followingIndex] = '?')//next character has to be '.' to separate the groups
                            && conditions[i..(i + head - 1)].IndexOf('.') = -1 //no dots inside, so can be placed
                            ) //is valid
                        |> List.map (fun i ->
                            let nextStart = i + head + 1 //take the group and separator
                            if nextStart < conditions.Length then
                                loop conditions[nextStart..] tail
                            else
                                //not enough data remaining to continue (if there is tail)
                                loop "" tail)
                        //|> (fun x -> printfn "%A" x; x)
                        |> List.sum
               ))
            
    loop conditions groups
        
        
let rowCount (row: string) =
    let conditions, groups = unfold5 row
    permCountMemoized conditions groups

let mutable cnt = 0

inputArray
|> Array.Parallel.mapi (fun i x ->
    //printfn $"%A{System.DateTime.Now}, index %i{i}: %A{x}"
    let rowCount = rowCount x
    cnt <- cnt + 1
    //printfn $"rowCount = %A{rowCount}, cnt = {cnt}"
    rowCount
    )
|> Array.sum 