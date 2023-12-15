#time

open System
open System.IO

let file = File.OpenRead("inputs/input15")
//let file = File.OpenRead("inputs/testData15")

let reader = new StreamReader(file)

let input =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Seq.collect (_.Split(','))
    |> Array.ofSeq
    
let hash (x: string) =
    x.ToCharArray()
    |> Array.fold (fun h ch ->
        ((h + (ch |> byte |> int32)) * 17 ) % 256
        ) 0


printfn $"PART 1: %A{input |> Array.map hash |> Array.sum}"

type Lens = { Label: string ; FocalLength: int }

type Operation =
    | RemoveLens of string
    | AddLens of Lens

let parseOperation (x: string) =
    if x.Contains "-" then
        x.Replace("-","") |> RemoveLens
    else
        let parts = x.Split("=")
        { Label = parts[0] ; FocalLength = Int32.Parse parts[1] }
        |> AddLens
    

let buckets : Lens list array = Array.init 256 (fun _ -> List.empty)

let folder (state: Lens list array) (x: string) =
    match parseOperation x with
    | RemoveLens label ->
        let bucket = state[hash label]
        state[hash label] <- bucket |> List.where (fun lens -> lens.Label <> label)
        state
    | AddLens lens ->
        let bucket = state[hash lens.Label]
        if bucket |> List.exists (fun x -> x.Label = lens.Label) then
            state[hash lens.Label] <-
                bucket
                |> List.map (fun existingLens ->
                    if existingLens.Label = lens.Label then lens else existingLens)
            state
        else
            state[hash lens.Label] <- bucket @ [ lens ]
            state    



input
|> Array.fold folder buckets
|> Array.mapi (fun i bucket ->
    bucket
    |> List.mapi (fun j lens ->
        (i+1) * (j+1) * lens.FocalLength
        )
    |> List.sum
    )
|> Array.sum


