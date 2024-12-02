module AoC2024.Day2.Solution

let validInterval a b =
    let diff = a - b |> abs
    diff >= 1 && diff <= 3

let getDirection a b =
    if a < b then 1 else -1

let rec isSafe (report: int list) (direction: int) =
    match (report, direction) with
    | [], _ -> true
    | [ _ ], _ -> true
    | head :: tail, 0 ->
        if validInterval head tail.Head then
            isSafe tail (getDirection head tail.Head) 
        else
            false
    | head :: tail, d ->
        if validInterval head tail.Head && d = (getDirection head tail.Head) then
            isSafe tail d 
        else
            false
            
let rec isSafe2 (past: int list) (report: int list) (direction: int) (skipped: bool) =
    match (report, direction) with
    | [], _ -> true
    | [ _ ], _ -> true
    | head :: tail, d ->
        let dir = getDirection head tail.Head
        
        if validInterval head tail.Head && (d = 0 || d = dir) then
            isSafe2 (head :: past) tail dir skipped
        else if skipped then
            false
        else
            let flippedPast = past |> List.rev
            let flippedPastTail = if past |> List.length > 0 then past.Tail |> List.rev else []
            
            isSafe2 [] (List.concat [ flippedPast; (head :: tail.Tail) ]) 0 true
            || isSafe2 [] (List.concat [ flippedPast; tail ]) 0 true
            || isSafe2 [] (List.concat [ flippedPastTail; (head :: tail) ]) 0 true

let solve1 (puzzleInput: string seq) =
    let reports =
        puzzleInput
        |> Seq.map (fun row -> row.Split(' '))
        |> Seq.map (fun row ->
            row
            |> Seq.map (fun value -> value |> int)
            |> Seq.toList)
        |> Seq.map (fun report -> (report, isSafe report 0))
    let result =
        reports
        |> Seq.countBy (fun (_, safe) -> safe)
        |> Seq.filter (fun (safe, _) -> safe = true)
        |> Seq.sumBy (fun (_, count) -> count)
    result.ToString()
    
let solve2 (puzzleInput: string seq) =
    let reports =
        puzzleInput
        |> Seq.map (fun row -> row.Split(' '))
        |> Seq.map (fun row ->
            row
            |> Seq.map (fun value -> value |> int)
            |> Seq.toList)
        |> Seq.map (fun report -> (report, isSafe2 [] report 0 false))
    let result =
        reports
        |> Seq.countBy (fun (_, safe) -> safe)
        |> Seq.filter (fun (safe, _) -> safe = true)
        |> Seq.sumBy (fun (_, count) -> count)
    result.ToString()