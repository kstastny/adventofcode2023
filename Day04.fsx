#time

open System
open System.IO
open System.Text.RegularExpressions
let inputDir = "./inputs"
let input = "input04"
//let input = "testData04"
use file = File.OpenRead(Path.Join(inputDir, input))
use reader = new StreamReader(file)

let numRegex = Regex("[0-9]*")

let getNumbers (x: string) =
    numRegex.Matches(x)
    |> Seq.choose (fun m ->
        if String.IsNullOrWhiteSpace m.Value |> not then
            m.Value |> Int32.Parse |> Some
        else
            None)
    |> Array.ofSeq
    
let cards =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Array.ofSeq

    
let pointsPerCard (card: string) =
    let numbers =
        (card.Split(':')[1]).Split('|')
        |> Array.map getNumbers
    
    let winningNumbers = numbers[0] |> Set.ofArray
    let elfsNumbers = numbers[1] |> Set.ofArray
    
    let points =
        Set.intersect winningNumbers elfsNumbers
    // printfn "%A, %A, %A" points winningNumbers elfsNumbers
    // printfn "%s: %i" card (Math.Pow(2, points.Count - 1 |> float) |> int)
    (Math.Pow(2, points.Count - 1 |> float) |> int)
    
cards
|> Array.map pointsPerCard
|> Array.sum
        
    
let scratchCardsWon (card: string) =
    let numbers =
        (card.Split(':')[1]).Split('|')
        |> Array.map getNumbers
    
    let winningNumbers = numbers[0] |> Set.ofArray
    let elfsNumbers = numbers[1] |> Set.ofArray
    
    let points =
        Set.intersect winningNumbers elfsNumbers
    points.Count
    

let cardCounts = Array.init cards.Length (fun _ -> 1)


cards
|> Array.iteri(fun i card ->
    let currentCopies = cardCounts[i]
    let nextCopies = scratchCardsWon card
    
    [1..currentCopies]
    |> List.iter (fun _ ->
        [1..nextCopies]
            |> List.iter (fun j -> cardCounts[i+j] <- cardCounts[i+j]+1)
        )
    ()
    )

cardCounts
|> Array.sum