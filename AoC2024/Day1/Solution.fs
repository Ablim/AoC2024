module AoC2024.Day1.Solution

let solve1 (puzzleInput : string seq) =
    let parsed =
        puzzleInput
        |> Seq.map (fun row -> row.Split(' '))
        |> Seq.map (fun row -> (row[0] |> int, row[3] |> int))
    let left =
        parsed
        |> Seq.map (fun (left, _) -> left)
        |> Seq.sort
    let right =
        parsed
        |> Seq.map (fun (_, right) -> right)
        |> Seq.sort
    let distanceSum =
        Seq.zip left right
        |> Seq.map (fun (left, right) -> left - right |> abs)
        |> Seq.sum
    distanceSum.ToString()
    
let solve2 (puzzleInput : string seq) =
    let parsed =
        puzzleInput
        |> Seq.map (fun row -> row.Split(' '))
        |> Seq.map (fun row -> (row[0] |> int, row[3] |> int))
    let left =
        parsed
        |> Seq.map (fun (left, _) -> left)
    let lookup =
        parsed
        |> Seq.map (fun (_, right) -> right)
        |> Seq.groupBy (fun x -> x)
        |> Seq.map (fun (key, group) -> (key, group |> Seq.length))
        |> Map
    let result =
        left
        |> Seq.map (fun x -> x * if lookup.ContainsKey x then lookup[x] else 0)
        |> Seq.sum
    result.ToString()