module AoC2024.Day3.Solution

open System.Text.RegularExpressions

let execute (input: string) =
    let parsed =
        input.Replace("mul(", "").Replace(")", "").Split(',')
        |> Array.map (fun x -> x |> int)
    
    parsed[0] * parsed[1]

let matchRegex pattern rows =
    seq {
        for m in Regex.Matches(rows, pattern) do
            yield m.Value, m.Index
    }    

let rec removeDonts (all: string list) remaining enabled =
    match all with
    | [] -> List.rev remaining
    | h :: t ->
        if h.StartsWith("mul") && enabled then
            removeDonts t (h :: remaining) enabled
        else if h.StartsWith("don") then
            removeDonts t remaining false
        else if h.StartsWith("do") then
            removeDonts t remaining true
        else
            removeDonts t remaining enabled

let solve1 (puzzleInput: string seq) =
    let oneLine =
        puzzleInput
        |> Seq.concat
        |> Seq.toArray
        |> System.String
    let operations = matchRegex "mul[(]\d{1,3},\d{1,3}[)]" oneLine
    
    let result =
        operations
        |> Seq.map (fun (x, _) -> x |> execute)
        |> Seq.sum
    result.ToString()
    
let solve2 (puzzleInput: string seq) =
    let oneLine =
        puzzleInput
        |> Seq.concat
        |> Seq.toArray
        |> System.String
        
    let operations = matchRegex "mul[(]\d{1,3},\d{1,3}[)]" oneLine
    let dos = matchRegex "do[(][)]" oneLine
    let donts = matchRegex "don't[(][)]" oneLine
    let all =
        Seq.concat [ operations; dos; donts ]
        |> Seq.toList
        |> List.sortBy snd
        |> List.map fst
    let remaining = removeDonts all [] true
    
    let result =
        remaining
        |> Seq.map execute
        |> Seq.sum
    result.ToString()