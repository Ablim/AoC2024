module Day2Tests

open Xunit
open AoC2024.Day2.Solution

let input =
    [|
        "7 6 4 2 1"
        "1 2 7 8 9"
        "9 7 6 2 1"
        "1 3 2 4 5"
        "8 6 4 4 1"
        "1 3 6 7 9"
    |]

[<Fact>]
let ``Part 1`` () =
    let result = solve1 input
    Assert.Equal("2", result)
    
[<Fact>]
let ``Part 2`` () =
    let result = solve2 input
    Assert.Equal("4", result)