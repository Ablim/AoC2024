module Program

open AoC2024.Day1.Solution

[<EntryPoint>]
let main args =
    let day = 1
    printfn $"Advent of Code - Day {day}"
    printfn ""
    
    let solution1 = solve1
    printfn "Part 1:"
    printfn $"{solution1}"
    printfn ""
    
    let solution2 = solve2
    printfn "Part 2:"
    printfn $"{solution2}"
    0