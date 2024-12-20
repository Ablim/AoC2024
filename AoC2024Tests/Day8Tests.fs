module Day8Tests

open Xunit
open AoC2024.Day8.Solution

let input =
    [|
        "............"
        "........0..."
        ".....0......"
        ".......0...."
        "....0......."
        "......A....."
        "............"
        "............"
        "........A..."
        ".........A.."
        "............"
        "............"
    |]
    
[<Fact>]
let ``Part 1`` () =
    let result = solve1 input
    Assert.Equal("14", result)
    
[<Fact>]
let ``Part 2`` () =
    let result = solve2 input
    Assert.Equal("34", result)