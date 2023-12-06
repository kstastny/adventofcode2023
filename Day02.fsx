open System
open System.IO
let inputDir = "./inputs"
let input = "input02"
//let input = "testData02"
use file = File.OpenRead(Path.Join(inputDir, input))
use reader = new StreamReader(file)


//game: (r,g,b)
let possibleSubset (r,g,b) =
    r <= 12 && g <= 13 && b <= 14
    
let possibleGame (gameNumber, subsets:(int*int*int) array) =
    subsets |> Array.forall possibleSubset
    
    
let parseSubset (x: string) =
    let split = x.Split(",")
    let red =
        split
        |> Array.tryFind (fun x -> x.Contains("red"))
        |> Option.map (fun x -> x.Replace(" red", "") |> Int32.Parse)
        |> Option.defaultValue 0
        
    let green =
        split
        |> Array.tryFind (fun x -> x.Contains("green"))
        |> Option.map (fun x -> x.Replace(" green", "") |> Int32.Parse)
        |> Option.defaultValue 0
        
    let blue =
        split
        |> Array.tryFind (fun x -> x.Contains("blue"))
        |> Option.map (fun x -> x.Replace(" blue", "") |> Int32.Parse)
        |> Option.defaultValue 0
    (red, green, blue)
    
let parseGame (x: string) =
    let split = x.Split(":")
    let gameNumber = split[0].Remove(0, 5) |> Int32.Parse
    let subsets =
        split[1].Split(";")
        |> Array.map parseSubset
    gameNumber, subsets 
    

let power (gameNumber, subsets:(int*int*int) array) =
    let r = subsets |> Array.map (fun (r, _, _) -> r) |> Array.max
    let g = subsets |> Array.map (fun (_, g, _) -> g) |> Array.max
    let b = subsets |> Array.map (fun (_, _, b) -> b) |> Array.max
    r*g*b
    
//STAR 1    
// seq {
//     while reader.EndOfStream |> not do
//         let game = reader.ReadLine() |> parseGame
//         if possibleGame game then yield game |> fst
// }
// |> Seq.sum

  
//STAR 2    
seq {
    while reader.EndOfStream |> not do
        let game = reader.ReadLine() |> parseGame
        let p = power game
        printfn "power = %i"  p
        yield p
}
|> Seq.sum   