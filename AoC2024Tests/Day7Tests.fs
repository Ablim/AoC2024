module Day7Tests

open Xunit
open AoC2024.Day7.Solution

let input =
    [|
        "190: 10 19"
        "3267: 81 40 27"
        "83: 17 5"
        "156: 15 6"
        "7290: 6 8 6 15"
        "161011: 16 10 13"
        "192: 17 8 14"
        "21037: 9 7 18 13"
        "292: 11 6 16 20"
    |]
    
[<Fact>]
let ``Part 1`` () =
    let result = solve1 input
    Assert.Equal("3749", result)
    
[<Fact>]
let ``Part 2`` () =
    let result = solve2 input
    Assert.Equal("11387", result)