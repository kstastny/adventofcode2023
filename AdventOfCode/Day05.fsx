#time

open System
open System.IO

let inputDir = "./AdventOfCode/inputs"
let input = "input05"
//let input = "testData05"
use file = File.OpenRead(Path.Join(inputDir, input))
use reader = new StreamReader(file)


let singleMap destinationStart sourceStart range =
    let offset = destinationStart - sourceStart
    fun x ->
        if x >= sourceStart && x <= sourceStart + range then
            x + offset |> Some
        else
            None

let parseMap (maps: string array) =
    let mapFunctions =
        maps
        |> Array.map (fun y ->
            let nums = y.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int64
            singleMap nums[0] nums[1] nums[2]
            )
    fun x ->
        mapFunctions
        |> Array.tryPick (fun fn -> fn x)
        |> Option.defaultValue x
    
    
let parseSeeds (seeds: string) =
    (seeds.Split(':')[1]).Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int64
    
let inputArray =
    seq {
        while reader.EndOfStream |> not do
            yield reader.ReadLine()
    }
    |> Array.ofSeq
    
let seeds = parseSeeds inputArray[0]

let maps =
    seq {
        //TODO without mutable
        let mutable mapName = ""
        for row in (inputArray[1..] |> Array.where (String.IsNullOrWhiteSpace >> not)) do
            if row.Contains "map" then
                mapName <- row
            else
                (mapName, row)
    }
    |> Seq.groupBy (fun (mapName, _) -> mapName)
    |> Seq.map (fun (mapName,maps) ->
            mapName.Replace(" map:", ""), (maps |> Seq.map snd |> Array.ofSeq |> parseMap)
        )
    |> Map.ofSeq
    
let seedToSoil = maps |> Map.find "seed-to-soil"    
let soilToFertilizer = maps |> Map.find "soil-to-fertilizer"    
let fertilizerToWater = maps |> Map.find "fertilizer-to-water"    
let waterToLight = maps |> Map.find "water-to-light"    
let lightToTemperature = maps |> Map.find "light-to-temperature"    
let temperatureToHumidity = maps |> Map.find "temperature-to-humidity"    
let humidityToLocation = maps |> Map.find "humidity-to-location"


let seedToLocation =
    seedToSoil
    >> soilToFertilizer
    >> fertilizerToWater
    >> waterToLight
    >> lightToTemperature
    >> temperatureToHumidity
    >> humidityToLocation

let minOfPairs (start, count) =
    let min =
        seq { start..(start+count) }
        |> Seq.map seedToLocation
        |> Seq.min
    printfn $"min of %i{start} is %i{min}"
    min
    

let seedPairs =
    seeds
    |> Array.mapi (fun index x ->
        if index % 2 = 1 then
            (seeds[index-1], x) |> Some
        else
            None)
    |> Array.choose id

    
seedPairs
|> Array.Parallel.map minOfPairs
|> Array.min
|> printfn "PART 2: %A"