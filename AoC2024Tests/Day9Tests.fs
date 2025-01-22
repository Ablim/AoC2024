module Day9Tests

open Xunit
open AoC2024.Day9.Solution

let input =
    [|
        "2333133121414131402"
    |]
    
[<Fact>]
let ``Part 1`` () =
    let result = solve1 input
    Assert.Equal("1928", result)
    
[<Fact>]
let ``Part 2`` () =
    let result = solve2 input
    Assert.Equal("34", result)