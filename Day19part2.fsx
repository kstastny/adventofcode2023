#time

open System

open System.IO

type Part = {
    X: int
    M: int
    A: int
    S: int
}

type Condition = {
    CheckedProperty: char  // one of 'xmas'
    //TODO typed
    Condition: char // > or <
    Threshold: int
}

type Step = 
    | Accepted
    | Rejected
    | NextWorkflow of string
    | Condition of Condition * Step

type Workflow = {
    Label: string
    Steps: Step list
}

// State traveled through the workflows
type State = {
    // list of conditions satisfied
    Conditions: Condition list
    
}

let reverseCondition (cond: Condition) = 
  {
      cond with
        Condition = if cond.Condition = '>' then '<' else '>'
        Threshold = if cond.Condition = '>' then cond.Threshold + 1 else cond.Threshold - 1 
  }
  
let minValue = 1
let maxValue = 4000
  
let countSatisfactions (cond: Condition) =
    if cond.Condition = '>' then
        maxValue - cond.Threshold
    else
        cond.Threshold - minValue
    |> int64
        
    
  
let c1a = { Condition = '>'; CheckedProperty = 'x'; Threshold = 1617 }  
let c1b = c1a |> reverseCondition  
let c2a = { Condition = '<'; CheckedProperty = 'x'; Threshold = 1617 }
let c2b = c2a |> reverseCondition

countSatisfactions c1a + countSatisfactions c1b

countSatisfactions c2a + countSatisfactions c2b


/// combinations for single property
let combinationsByProperty (conditions: Condition list) =
    match conditions with
    | [] -> 4000L
    | [ head ] -> countSatisfactions head
    | _ ->
        let smallerThan =
            match conditions |> List.where (fun c -> c.Condition = '<') with
            | [] -> None
            | y -> y |> List.map _.Threshold |> List.min |> Some
        let greaterThan = 
            match conditions |> List.where (fun c -> c.Condition = '>') with
            | [] -> None
            | y -> y |> List.map _.Threshold |> List.max |> Some
            
        //printfn "smaller %A greater %A" smallerThan greaterThan
        match smallerThan, greaterThan with
        | Some s, Some g -> Math.Max(0, s - g - 1) |> int64
        | Some s, None -> s - minValue |> int64
        | None, Some g -> maxValue - g  |> int64
        | None, None -> failwithf "combinationsByProperty logic error"
        
        
combinationsByProperty [ c1a; c1a; c1a; { c1a with Threshold = 2500 }; c1a |> reverseCondition]
countSatisfactions c1a

let countCombinations (conditions: Condition list) =
    let condMap =
        conditions |> List.groupBy (_.CheckedProperty) |> Map.ofList
    
    let xCombinations =
        condMap
        |> Map.tryFind 'x'
        |> Option.map combinationsByProperty
        |> Option.defaultValue 4000L
        
    let mCombinations =
        condMap
        |> Map.tryFind 'm'
        |> Option.map combinationsByProperty
        |> Option.defaultValue 4000L
        
    let aCombinations =
        condMap
        |> Map.tryFind 'a'
        |> Option.map combinationsByProperty
        |> Option.defaultValue 4000L
        
    let sCombinations =
        condMap
        |> Map.tryFind 's'
        |> Option.map combinationsByProperty
        |> Option.defaultValue 4000L
        
    //printfn $"{xCombinations} * {mCombinations} * {aCombinations} * {sCombinations}"
    xCombinations * mCombinations * aCombinations * sCombinations
    

countCombinations [ c1a ; {c1a with CheckedProperty = 's' } ]    
  
  
let parseStep (workflowStep: string) : Step =
    let parseStepResult (result: string) =
        match result with
        | "A" -> Accepted 
        | "R" -> Rejected 
        | x -> NextWorkflow x 

    
    match workflowStep with
    | x when x.Contains ":" ->
        let split = x.Split(':')
        
        let result = split[1] |> parseStepResult
        let condition = 
            {
                CheckedProperty = split[0][0] // one of 'xmas'
                Condition = split[0][1] // > or <
                Threshold = split[0][2..] |> Int32.Parse 
            }
        Condition (condition, result)
            
    | x -> parseStepResult x
    

let parseWorkflow (workflowSpec: string) =
    let specParts = workflowSpec[..workflowSpec.Length - 2].Split("{")
    
    {
        Label = specParts[0]
        Steps = specParts[1].Split(",") |> Array.map parseStep |> List.ofArray
    }



let parsePart (partSpec: string) =
    let partParts =
        partSpec[1..partSpec.Length - 2].Split(',')
        |> Array.map (fun x -> x[0], x[2..])
        |> Map.ofArray
    
    {
        X = partParts |> Map.find 'x' |> Int32.Parse
        M = partParts |> Map.find 'm' |> Int32.Parse
        A = partParts |> Map.find 'a' |> Int32.Parse
        S = partParts |> Map.find 's' |> Int32.Parse
    }


let solve2 (filename: string) =
    
    let file = File.OpenRead filename

    let reader = new StreamReader(file)

    let workflows =    
        let input = 
            seq {
                while reader.EndOfStream |> not do
                    yield reader.ReadLine()
            }
            |> Array.ofSeq
        let separator =
            input
            |> Array.indexed
            |> Array.find (fun (_,x) -> String.IsNullOrWhiteSpace x)
            |> fst
        
        input[..separator - 1] |> Array.map parseWorkflow |> Array.map (fun w -> w.Label, w) |> Map.ofArray
        //input[separator + 1..] |> Array.map parsePart parts are not used
        
    file.Close()
    
    //workflows |> Map.find "in"
    
    let rec getFinalConditions (state: State) (steps: Step list) =
        match steps with
        | [  ] -> failwith "EMPTY STEPS ERROR"
        | head::tail ->
            match head with
            | Accepted -> [ Accepted, state ]
            | Rejected -> [ Rejected, state ]
            | NextWorkflow workflowLabel ->
                let w = workflows |> Map.find workflowLabel
                getFinalConditions state w.Steps
            | Condition (cond, condSteps) ->
                let matchCondition =
                    getFinalConditions { Conditions = cond::state.Conditions } [ condSteps ]
                let doNotMatchCondition =
                    let revCondition = cond |> reverseCondition
                    getFinalConditions { Conditions = revCondition::state.Conditions } tail
                matchCondition @ doNotMatchCondition
    
    let startWorkflow = workflows |> Map.find "in"
    getFinalConditions {Conditions = [] } startWorkflow.Steps
    |> List.choose (fun (step, state) ->
        match step with
        | Accepted -> state |> Some
        | _ -> None
        )
    |> List.sumBy (fun state ->
        countCombinations state.Conditions
        )
  
    
solve2 "inputs/testData19" //should be 167409079868000L    
solve2 "inputs/input19" // 103557657654583L