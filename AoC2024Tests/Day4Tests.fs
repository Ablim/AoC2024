module Day4Tests

open Xunit
open AoC2024.Day4.Solution

let input =
    [|
        "MMMSXXMASM"
        "MSAMXMSMSA"
        "AMXSXMAAMM"
        "MSAMASMSMX"
        "XMASAMXAMM"
        "XXAMMXXAMA"
        "SMSMSASXSS"
        "SAXAMASAAA"
        "MAMMMXMMMM"
        "MXMXAXMASX"
    |]
    
[<Fact>]
let ``Part 1`` () =
    let result = solve1 input
    Assert.Equal("18", result)
    
[<Fact>]
let ``Part 2`` () =
    let result = solve2 input
    Assert.Equal("9", result)