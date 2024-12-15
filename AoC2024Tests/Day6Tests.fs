module Day6Tests

open Xunit
open AoC2024.Day6.Solution

let input =
    [|
        "....#....."
        ".........#"
        ".........."
        "..#......."
        ".......#.."
        ".........."
        ".#..^....."
        "........#."
        "#........."
        "......#..."
    |]
    
[<Fact>]
let ``Part 1`` () =
    let result = solve1 input
    Assert.Equal("41", result)
    
[<Fact>]
let ``Part 2`` () =
    let result = solve2 input
    Assert.Equal("6", result)