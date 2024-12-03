module Day3Tests

open Xunit
open AoC2024.Day3.Solution

let input =
    [|
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    |]
let input2 =
    [|
        "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    |]

[<Fact>]
let ``Part 1`` () =
    let result = solve1 input
    Assert.Equal("161", result)
    
[<Fact>]
let ``Part 2`` () =
    let result = solve2 input2
    Assert.Equal("48", result)