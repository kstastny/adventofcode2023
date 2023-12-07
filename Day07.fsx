#time

open System
open System.IO
open System.Text.RegularExpressions

// let inputDir = "./inputs"
// //let input = "input07"
// let input = "testData07"
//
// Path.Join(inputDir; input)
// let file = File.OpenRead(Path.Join(inputDir; input))
let file = File.OpenRead("inputs/input07")
//let file = File.OpenRead("inputs/testData07")
let reader = new StreamReader(file)

let inputArray =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Array.ofSeq  

type HandType =
    | FiveOfAKind
    | FourOfAKind
    | Fullhouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard
    
let strength = function
    | FiveOfAKind -> 6
    | FourOfAKind -> 5
    | Fullhouse -> 4
    | ThreeOfAKind  -> 3
    | TwoPair -> 2
    | OnePair  -> 1
    | HighCard -> 0 
    

let cardValues =
    ['2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A']
    |> List.indexed
    |> List.map (fun (x,y) -> y,x)
    |> Map.ofList
    

type Hand =
    {
        Cards: int[]
        CardCounts: int[]
        Type: HandType
    }
    
    
    

let parseHand (x: string) =
    let cards = x.Split(' ')[0] |> (fun x -> x.ToCharArray() |> Array.map (fun y -> cardValues |> Map.find y))
    let bid = x.Split(' ')[1] |> Int32.Parse
    
    let cardCounts =
        cards
        |> Array.groupBy id
        |> Array.map (fun (_,x) -> x.Length)
        |> Array.sortDescending
    {
        Cards = cards
        CardCounts = cardCounts 
        Type = 
            match cardCounts with
            | x when x[0] = 5 -> FiveOfAKind
            | x when x[0] = 4 -> FourOfAKind
            | x when x[0] = 3 && x[1] = 2 -> Fullhouse
            | x when x[0] = 3 -> ThreeOfAKind
            | x when x[0] = 2 && x[1] = 2 -> TwoPair
            | x when x[0] = 2 -> OnePair
            | _ -> HighCard
    },
    bid
    
    


    
  
    
    
let hands =
    inputArray
    |> Array.map parseHand
    |> Array.sortWith (fun (x1, _) (x2, _) ->
        match strength x1.Type, strength x2.Type with
        | x,y when x > y -> -1
        | x,y when x < y -> 1
        | _ ->
            match x1.Cards, x2.Cards with
            | x,y when x > y -> -1
            | x,y when x < y -> 1
            | _ -> 0
        )
    |> Array.rev
    |> Array.mapi (fun i x ->
        //printfn $"i = %i{i}, hand = %A{x}, score = %A{(i+1) * (snd x)}"
        (i+1) * (snd x)
        )
    |> Array.fold (+) 0
    
    
let cardValuesWithJokers =
    ['J'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T';  'Q'; 'K'; 'A']
    |> List.indexed
    |> List.map (fun (x,y) -> y,x)
    |> Map.ofList
    
let parseHandWithJokers (x: string) =
    let cards = x.Split(' ')[0] |> (fun x -> x.ToCharArray() |> Array.map (fun y -> cardValuesWithJokers |> Map.find y))
    let bid = x.Split(' ')[1] |> Int32.Parse
    
    //TODO clean
    let jokerCount = cards |> Array.where (fun x -> x = 0) |> Array.length
    
    let cardCountsExceptJokers =
        cards
        |> Array.where (fun x -> x > 0)
        |> Array.groupBy id
        |> Array.map (fun (_,x) -> x.Length)
        |> Array.sortDescending
    {
        Cards = cards
        CardCounts = cardCountsExceptJokers 
        Type = 
            match cardCountsExceptJokers with
            | x when x.Length = 0 -> FiveOfAKind //just jokers
            | x when x[0] + jokerCount = 5 -> FiveOfAKind
            | x when x[0] + jokerCount = 4 -> FourOfAKind
            | x when x[0] + jokerCount = 3 && x[1] = 2 -> Fullhouse
            | x when x[0] + jokerCount = 3 -> ThreeOfAKind
            | x when x[0] + jokerCount = 2 && x[1] = 2 -> TwoPair
            | x when x[0] + jokerCount = 2 -> OnePair
            | _ -> HighCard
    },
    bid    
        
        
// 249860792 too high        
let handsWithJokers =
    inputArray
    |> Array.map parseHandWithJokers
    |> Array.sortWith (fun (x1, _) (x2, _) ->
        match strength x1.Type, strength x2.Type with
        | x,y when x > y -> -1
        | x,y when x < y -> 1
        | _ ->
            match x1.Cards, x2.Cards with
            | x,y when x > y -> -1
            | x,y when x < y -> 1
            | _ -> 0
        )
    |> Array.rev
    |> Array.mapi (fun i x ->
        //printfn $"i = %i{i}, hand = %A{x}, score = %A{(i+1) * (snd x)}"
        (i+1) * (snd x)
        )
    |> Array.fold (+) 0        