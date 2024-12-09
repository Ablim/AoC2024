module Day5Tests

open Xunit
open AoC2024.Day5.Solution

let input =
    [|
        "47|53"
        "97|13"
        "97|61"
        "97|47"
        "75|29"
        "61|13"
        "75|53"
        "29|13"
        "97|29"
        "53|29"
        "61|53"
        "97|53"
        "61|29"
        "47|13"
        "75|47"
        "97|75"
        "47|61"
        "75|61"
        "47|29"
        "75|13"
        "53|13"
        ""
        "75,47,61,53,29"
        "97,61,53,29,13"
        "75,29,13"
        "75,97,47,61,53"
        "61,13,29"
        "97,13,75,29,47"
    |]
    
[<Fact>]
let ``Part 1`` () =
    let result = solve1 input
    Assert.Equal("143", result)
    
[<Fact>]
let ``Part 2`` () =
    let result = solve2 input
    Assert.Equal("123", result)