module Day1Tests

open Xunit
open AoC2024.Day1.Solution

[<Fact>]
let ``Part 1`` () =
    let input =
        [|
            "3   4"
            "4   3"
            "2   5"
            "1   3"
            "3   9"
            "3   3"
        |]
    let result = solve1 input
    Assert.Equal("11", result)
    
[<Fact>]
let ``Part 2`` () =
    let input =
        [|
            "3   4"
            "4   3"
            "2   5"
            "1   3"
            "3   9"
            "3   3"
        |]
    let result = solve2 input
    Assert.Equal("31", result)