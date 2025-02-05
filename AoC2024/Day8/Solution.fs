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
    
let getAntiNodesBetween2 antenna1 antenna2 height width =
    // from a1 through a2 until the limit, get all nodes
    let row1, col1, _ = antenna1
    let row2, col2, _ = antenna2
    let deltaRow = row2 - row1
    let deltaCol = col2 - col1
    let mutable factor = 0
    
    seq {
        while row1 + deltaRow * factor >= 0
            && row1 + deltaRow * factor < height
            && col1 + deltaCol * factor >= 0
            && col1 + deltaCol * factor < width do
            
            let temp = factor
            factor <- factor + 1
            (row1 + deltaRow * temp, col1 + deltaCol * temp)
    }
    |> Seq.toList

let getAntiNodes2 (map: char array array) =
    let height = map.Length
    let width = map[0].Length
    let antennas =
        seq {
            for row in 0..(height - 1) do
                for col in 0..(width - 1) do
                    if map[row][col] <> '.' then
                        (row, col, map[row][col])
        }
    
    seq {
        for a1 in antennas do
            for a2 in antennas do
                let _, _, freq1 = a1
                let _, _, freq2 = a2
                
                if a1 <> a2 && freq1 = freq2 then
                    getAntiNodesBetween2 a1 a2 height width
    }
    |> List.concat
    |> List.distinct

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