#time

open System

open System.IO

type Part = {
    X: int
    M: int
    A: int
    S: int
}

type StepResult =
    | Accepted
    | Rejected
    | NextWorkflow of string
    | NotMatched

type Workflow = {
    Label: string
    Steps: (Part -> StepResult) list
}



let parseStep (workflowStep: string) : Part -> StepResult =
    let parseStepResult (result: string) =
        match result with
        | "A" -> Accepted 
        | "R" -> Rejected 
        | x -> NextWorkflow x 

    
    match workflowStep with
    | x when x.Contains ":" ->
        let split = x.Split(':')
        
        let result = split[1] |> parseStepResult
        let checkedProperty = split[0][0] // one of 'xmas'
        let condition = split[0][1] // > or <
        let threshold = split[0][2..] |> Int32.Parse
        
        fun part ->
            match checkedProperty, condition with
            | 'x', '>' -> if part.X > threshold then result else NotMatched
            | 'x', '<' -> if part.X < threshold then result else NotMatched
            | 'm', '>' -> if part.M > threshold then result else NotMatched
            | 'm', '<' -> if part.M < threshold then result else NotMatched
            | 'a', '>' -> if part.A > threshold then result else NotMatched
            | 'a', '<' -> if part.A < threshold then result else NotMatched
            | 's', '>' -> if part.S > threshold then result else NotMatched
            | 's', '<' -> if part.S < threshold then result else NotMatched
            | part, condition -> failwithf $"Unexpected part %c{part} or condition %c{condition}"
            
    | x -> fun _ -> parseStepResult x
    

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


let evaluate (workflows: Map<string, Workflow>) (part: Part) =
    
    let rec loop (w: Workflow) =
        let workFlowResult = 
            w.Steps
            |> Seq.map (fun step -> part |> step)
            |> Seq.find (fun result ->
                match result with
                | NotMatched -> false
                | _ -> true
                )
        match workFlowResult with
        | Accepted -> Accepted
        | Rejected -> Rejected
        | NotMatched -> failwithf $"ERROR: Part %A{part} did not match anything in workflow %s{w.Label}"
        | NextWorkflow x -> loop (workflows |> Map.find x)
        
    workflows |> Map.find "in" |> loop



    


let solve1 (filename: string) =
    
    let file = File.OpenRead filename

    let reader = new StreamReader(file)

    let workflows, parts =    
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
        
        input[..separator - 1] |> Array.map parseWorkflow |> Array.map (fun w -> w.Label, w) |> Map.ofArray,
        input[separator + 1..] |> Array.map parsePart
        
    file.Close()

    parts
    |> Array.choose (fun part ->
        match evaluate workflows part with
        | Accepted -> Some part
        | _ -> None)
    |> Array.sumBy (fun part ->
        part.X + part.M + part.A + part.S
        )
    
solve1 "inputs/testData19"    
solve1 "inputs/input19"    