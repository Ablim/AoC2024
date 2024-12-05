module AoC2024.Day4.Solution

let countXmas (board: char[][]) (position: int * int) =
    let row, col = position
    let height = board.Length
    let width = board[0].Length
    
    let right =
        seq {
            for i in 0..3 do
                if col + i < width then
                    board[row][col + i]
        }
    let left =
        seq {
            for i in 0..3 do
                if col - i >= 0 then
                    board[row][col - i]
        }
    let up =
        seq {
            for i in 0..3 do
                if row - i >= 0 then
                    board[row - i][col]
        }
    let down =
        seq {
            for i in 0..3 do
                if row + i < height then
                    board[row + i][col]
        }
    let upRight =
        seq {
            for i in 0..3 do
                if row - i >= 0 && col + i < width then
                    board[row - i][col + i]
        }
    let upLeft =
        seq {
            for i in 0..3 do
                if row - i >= 0 && col - i >= 0 then
                    board[row - i][col - i]
        }
    let downRight =
        seq {
            for i in 0..3 do
                if row + i < height && col + i < width then
                    board[row + i][col + i]
        }
    let downLeft =
        seq {
            for i in 0..3 do
                if row + i < height && col - i >= 0 then
                    board[row + i][col - i]
        }
        
    let all =
        [ right; left; up; down; upRight; upLeft; downRight; downLeft ]
        |> List.map (fun x -> x |> Seq.toArray |> System.String)
        |> List.filter (fun x -> x = "XMAS")
    all.Length
    
let countXmas2 (board: char[][]) (position: int * int) =
    let row, col = position
    let height = board.Length
    let width = board[0].Length
    
    let upRight =
        seq {
            for i in -1..1 do
                if row - i >= 0 && row - i < height && col + i >= 0 && col + i < width then
                    board[row - i][col + i]
        }
    let upLeft =
        seq {
            for i in -1..1 do
                if row - i >= 0 && row - i < height && col - i >= 0 && col - i < width then
                    board[row - i][col - i]
        }
        
    let all =
        [ upRight; upLeft; ]
        |> List.map (fun x -> x |> Seq.toArray |> System.String)
        |> List.filter (fun x -> x = "MAS" || x = "SAM")
    if all.Length = 2 then
        1
    else
        0

let solve1 (puzzleInput: string seq) =
    let board =
        puzzleInput
        |> Seq.map (fun x -> x.ToCharArray())
        |> Seq.toArray
    let xs =
        seq {
            for r in 0..board.Length - 1 do
                    for c in 0..board[r].Length - 1 do
                        if board[r][c] = 'X' then yield (r, c)
        }
        |> Seq.toArray
    let result =
        seq {
            for i in 0..xs.Length - 1 do
                countXmas board xs[i]
        }
        |> Seq.sum
    
    result.ToString()
    
let solve2 (puzzleInput: string seq) =
    let board =
        puzzleInput
        |> Seq.map (fun x -> x.ToCharArray())
        |> Seq.toArray
    let xs =
        seq {
            for r in 0..board.Length - 1 do
                    for c in 0..board[r].Length - 1 do
                        if board[r][c] = 'A' then yield (r, c)
        }
        |> Seq.toArray
    let result =
        seq {
            for i in 0..xs.Length - 1 do
                countXmas2 board xs[i]
        }
        |> Seq.sum
    
    result.ToString()