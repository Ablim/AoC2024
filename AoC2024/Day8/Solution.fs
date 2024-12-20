module AoC2024.Day8.Solution

let getAntiNodesBetween rowA colA rowB colB =
    let deltaRow = rowA - rowB
    let deltaCol = colA - colB
    let rowA' = rowA + deltaRow
    let rowB' = rowB - deltaRow
    let colA' = colA + deltaCol
    let colB' = colB - deltaCol
    [ (rowA', colA'); (rowB', colB') ]

let getAntiNodes (map: char array array) =
    let antennas =
        seq {
            for row in 0..map.Length - 1 do
                for col in 0..map[0].Length - 1 do
                    if map[row][col] <> '.' then
                        (row, col, map[row][col])
        }
    seq {
        for a1 in antennas do
            for a2 in antennas do
                let row1, col1, freq1 = a1
                let row2, col2, freq2 = a2
                
                if a1 <> a2 && freq1 = freq2 then
                    getAntiNodesBetween row1 col1 row2 col2
    }
    |> List.concat
    |> List.distinct
    |> List.filter (fun (r, c) -> r >= 0 && r < map.Length && c >= 0 && c < map[0].Length)
    
let getAntiNodesBetween2 rowA colA rowB colB lastRow lastCol =
    let deltaRow = rowA - rowB
    let deltaCol = colA - colB
    let rowA' = rowA + deltaRow
    let rowB' = rowB - deltaRow
    let colA' = colA + deltaCol
    let colB' = colB - deltaCol
    [ (rowA', colA'); (rowB', colB') ]

let getAntiNodes2 (map: char array array) =
    let antennas =
        seq {
            for row in 0..map.Length - 1 do
                for col in 0..map[0].Length - 1 do
                    if map[row][col] <> '.' then
                        (row, col, map[row][col])
        }
    
    seq {
        for a1 in antennas do
            for a2 in antennas do
                let row1, col1, freq1 = a1
                let row2, col2, freq2 = a2
                
                if a1 <> a2 && freq1 = freq2 then
                    getAntiNodesBetween2 row1 col1 row2 col2 (map.Length - 1) (map[0].Length - 1)
    }
    |> List.concat
    |> List.distinct
    |> List.filter (fun (r, c) -> r >= 0 && r < map.Length && c >= 0 && c < map[0].Length)

let solve1 (puzzleInput: string seq) =
    let antennas =
        puzzleInput
        |> Seq.map (fun x -> x.ToCharArray())
        |> Seq.toArray
    
    let result =
        getAntiNodes antennas
        |> List.length
    result.ToString()
    
let solve2 (puzzleInput: string seq) =
    let antennas =
        puzzleInput
        |> Seq.map (fun x -> x.ToCharArray())
        |> Seq.toArray
    
    let result =
        getAntiNodes2 antennas
        |> List.length
    result.ToString()