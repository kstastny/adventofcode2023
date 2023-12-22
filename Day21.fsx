#time

open System
open System.IO


let file = File.OpenRead("inputs/input21")
//let file = File.OpenRead("inputs/testData21")
//let file = File.OpenRead("inputs/testData21b")
//let file = File.OpenRead("inputs/testData21c")
//let file = File.OpenRead("inputs/testData21e")
//let file = File.OpenRead("inputs/testData21f")

let reader = new StreamReader(file)




let s, inputGrid =
    let input = 
        seq {
            while reader.EndOfStream |> not do
                yield reader.ReadLine().ToCharArray()
        }
        |> Array.ofSeq
    
    let startPosition =
        input
        |> Array.indexed
        |> Array.collect (fun (i, x) -> x |> Array.indexed |> Array.map (fun (j, y) -> (i,j,y)))
        |> Array.find (fun (i,j,x) -> x = 'S')
        |> (fun (i,j,_) -> (i,j))
    input[fst startPosition][snd startPosition] <- '.'
    startPosition, input
        
// PART 1
// let nextSteps (grid: char array array) stepList =
//     [
//         for i,j in stepList do
//             if i - 1 >= 0 && grid[i - 1][j] <> '#' then (i - 1, j)
//             if j - 1 >= 0 && grid[i][j - 1] <> '#' then (i, j - 1)
//             if i + 1 < grid.Length  && grid[i + 1][j] <> '#' then (i + 1, j )
//             if j + 1 < grid[i].Length && grid[i][j + 1] <> '#' then (i, j + 1)
//     ]
//     |> List.distinct
//   
//   
// let next = nextSteps inputGrid  
// let rec loop stepCount possiblePositions =
//     if stepCount = 0 then possiblePositions
//     else
//         possiblePositions
//         |> next
//         |> loop (stepCount - 1)
//
// loop 64 [ s ] |> List.length



let rec fixCoord gridLength n =
    if n < 0 then
        fixCoord gridLength (n + gridLength)
    else
        n % gridLength


let nextSteps2 (grid: char array array) stepList =
    [
        for i,j in stepList do
            let iMod = fixCoord grid.Length i
            let iModMinus = fixCoord grid.Length (i-1)
            let iModPlus = fixCoord grid.Length (i+1)
            let jMod = fixCoord grid[0].Length j
            let jModMinus = fixCoord grid[0].Length (j-1)
            let jModPlus = fixCoord grid[0].Length (j+1)
            if grid[iModMinus][jMod] <> '#' then (i - 1, j)
            if grid[iMod][jModMinus] <> '#' then (i, j - 1)
            if grid[iModPlus][jMod] <> '#' then (i + 1, j )
            if grid[iMod][jModPlus] <> '#' then (i, j + 1)
    ]
    |> List.distinct
    
    
// let next = nextSteps2 inputGrid  
// let rec loop stepCount possiblePositions =
//     if stepCount = 0 then possiblePositions
//     else
//         possiblePositions
//         |> next
//         |> loop (stepCount - 1)
//
// loop 64 [ s ] |> List.length    
    
    
let countPositionsBrute stepCount startPosition =
    
    let mutable p = [ startPosition ]
    seq {
        yield (0, p |> List.length)
        for step in [1 .. stepCount] do
            p <- nextSteps2 inputGrid p
            yield (step, p |> List.length)
    }
    
let differences (x: int array) = 
    x
    |> Array.pairwise
    |> Array.map (fun (x,y) -> y - x)
    
    

    
    
let possiblePositions =
    Array.init inputGrid.Length (fun i ->
        Array.init inputGrid[0].Length (fun j ->
            if i = 5 && j = 5 then 1L else 0L
            )
        )
   
   
let rec getPeriod (testArray: int array) n =
    //printfn "period %i" n
    if n > (testArray.Length / 2) then
        None
    elif testArray[0..n - 1] = testArray[n .. 2*n - 1] then
        n |> Some
    else
        getPeriod testArray (n+1)
        

// https://en.wikipedia.org/wiki/Finite_difference second-order central, explained by chatGPT
let derivative2 (n: int array) =
    [|
        //approx
        n[1] - 2*n[0]+n[1]
        for i in [1..n.Length - 2] do
            n[i+1] - 2*n[i] + n[i - 1]
        n[n.Length - 2] - 2*n[n.Length - 1] + n[n.Length - 2]
    |]
    
    
// [|1..10|] |> Array.map (fun i -> i*i) |> derivative2     

// countPositionsBrute 0 s //1
// countPositionsBrute 6 s |> Seq.last //16
// countPositionsBrute 10 s |> Seq.last  //50
// countPositionsBrute 50 s |> Seq.last // 1594 
// countPositionsBrute 100 s |> Seq.last // 6536 
// let arr =
//     countPositionsBrute 500 s // 167004, takes 11 seconds 
//     |> Array.ofSeq
    
let arr =
    countPositionsBrute 500 s  
    |> Array.ofSeq    
    
arr |> Array.last
let a = arr |> Array.map snd
a

arr
let d1 = a |> differences
//let d2 = a |> derivative2
let d2 = d1 |> differences



// let d2b = d1 |> differences //is the same I had before, no periodic...
//
// d2[2..]

// 1; 2; 4; 6; 9; 13; 16; 22; 30; 41; 50; 63; 74; 89; 99; 115; 129; 145;
// x; 1; 2; 2; 3; 4; 3; 6; 8; 11; 9; 13; 11; 15; 10; 16; 14; 16; 20; 27; 24; 18;
//       1; 0; 1; 1; -1; 3; 2; 3; -2; 4; -2; 4; -5; 6; -2; 2; 4; 7; -3; -6; 9; 6;
//       1; 0; 1; 1; -1; 3; 2; 3; -2; 4; -2; 4; -5; 6; -2; 2; 4; 7; -3; -6; 9; 6;

// getPeriod d2b 2
//
// getPeriod d2[2..] 8

//TODO this is the period. after each cycle, these get added to the speed - works for testData21f, does not for testData21
// for i in [0..11] do
//     printfn $"%i{i}: %i{d2[i+11]-d2[i]} {d2[i+22]-d2[i+11]} {d2[i+33]-d2[i+22]} {d2[i+44]-d2[i+33]}"
    
////////////////////////////////////////////////    
//TODO works for input21    
let period = 131
let pArray = 
    [|
       for i in [0..period - 1] do
           printfn $"%i{i}: %i{d2[i+period]-d2[i]} {d2[i+2*period]-d2[i+period]}"
           d2[i+2*period]-d2[i+period]
    |]
    
pArray.Length

//same as d2[i]
let getAcc i = (i/period) * pArray[i % period] + d2[i % period]

let checkAcc i =
    printfn $"%i{i}: %i{d2[i]} vs %i{getAcc i}"

[350..360] |> List.iter (fun i -> checkAcc i)

let nextValue i (vi: int64) (di: int64) =
    vi + di, di + (int64 (getAcc i))
    
let i = 130     
let vi = a[i] |> int64
let di = d1[i] |> int64

let targetStep = 26501365

let gen =
    seq {
        let mutable step = i
        let mutable valueI = vi
        let mutable diffI = di
        while step < targetStep do
            let (vi2, di2) = nextValue step valueI diffI
            
            valueI <- vi2
            diffI <- di2
            step <- step + 1
            yield (step, valueI)
    }

let xx = gen |> Seq.take 10 |> Array.ofSeq

let finalVal = gen |> Seq.last // 637087163925555L

//check values
xx
|> Array.iter(fun (i,v) ->
    printfn $"%i{i}: %i{a[i]} vs %i{v}"
    )
///////////////////////////////
    
    
//
// d2[131] - d2[0] //first repeating 
// d2[132] - d2[1] 
    
//TODO next steps - get the periodArray 131
//TODO calculate - next number is i*periodArray
// 26501365
26501365 / 131 = 202300
// a
// d1 // a[1] = a[0] + d1[0]
// a[5] = a[4] + d1[4]
// d1[132]
//
// d1[130..135] //index in d1 is what has to be added to get next num
// a[130..135] |> differences //131-130; 132-131; 133-132; etc.
// let added = pArray[132 % period] * (132/period) + d1[130]
//
// pArray[125..] |> Array.last
//let getAcc i = pArray[i % period] //* (i/period) //
// getAcc 130 // 6, d1[132] - d1[131]
// getAcc 131 // - 2, d1[133] - d1[132]
// getAcc 132 // 4, d1[134] - d1[133]
// getAcc 133 // -4, d1[135] - d1[134]
// getAcc 134 // 2
// getAcc 135 // 1


//
// d1[132]
// a[133] - a[132] //d1[132]; a[132] + d1[132] = a[133]; d1[134] = d1[133] + getAcc 132 //acc is one lower
//
// let stepNum = 132
// let value = a[stepNum]
// let delta = d1[stepNum] //delta - what to get for next value
//
// let nextValue v d currentStep =
//     let delta = d + getAcc (currentStep - 1)
//     v + d, delta //value and delta to get next value
//     
// d1[134] = d1[133] + getAcc 132    
// d1[133] = d1[132] + getAcc 131    
//    
// nextValue value delta stepNum
// nextValue 16289 243 133
// nextValue 16532 247 134
// a[134]
//
// a[stepNum]
// a[stepNum + 1]
//     
// pArray    
//     
// let targetStep = 140
//
// a[133..140]
//
// let rec loop inputValue inputDelta inputStep =
//     if inputStep = targetStep then
//         inputValue
//     else
//         let newValue, deltaForNext = nextValue inputValue inputDelta inputStep
//         printfn $"%i{inputStep + 1}: %A{newValue}, %A{deltaForNext}"
//         loop newValue deltaForNext (inputStep + 1)
// loop value delta stepNum
// //TODO a[136] counted incorrectly (was 17022 instead of 17028)
// d1[135] - d1[134]
// getAcc 133 //is 2, but should be 
//
// a[138]
// a[139]
// a[targetStep]
//
//
// //TODO how to get d1[i] = d1[i - 1] + added
//
// a[131]
// a[132]
// a[132] - a[131]

//
// countPositionsBrute 64 s |> Seq.last
//
//
// let a2 = a |> Array.indexed |> Array.choose (fun (i,n) -> if i % 2 = 0 then Some n else None)
// let a2d1 = a2 |> differences
// let a2d2 = a2 |> derivative2
//
// getPeriod a2d2 2
//
// a

// let q = Array.init 501 (fun i -> (i+1)*(i+1)) 
//  
// let qd = Array.zip q a |> Array.map (fun (q,a) -> q - a)
//
// qd[1..] //step 12 reaches same position as step 11 if it could go up. step 10 reaches from up
//
// [|
//   for i in [0..10] do
//       yield qd[i], qd[i+10] - qd[i]
// |]
    
 
// |> Array.length

//let arr1000 = countPositionsBrute 1000 s |> Array.ofSeq // 668697
// countPositionsBrute 5000 s // 16733044



// // test data
// countPositions 0 s //1
// countPositions 6 s //16
// countPositions 10 s //50
// countPositions 50 s // 1594 
// countPositions 100 s // 6536 
// countPositions 500 s // 167004, takes 11 seconds 
// countPositions 1000 s // 668697
// countPositions 5000 s // 16733044
//
// // input
//countPositions 26501365 s // ???