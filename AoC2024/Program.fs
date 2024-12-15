module Program

open System
open System.IO
open System.Net
open System.Net.Http
open AoC2024.Day6.Solution

let download day =
    async {
        let filename = $"puzzleInput{day}.txt"
        let fileExists = File.Exists(filename) 
        
        if not fileExists then 
            let! cookie = File.ReadAllTextAsync("cookie.txt") |> Async.AwaitTask
            let container = CookieContainer();
            container.Add(Cookie("session", cookie, "", ".adventofcode.com"))
            
            let handler = new HttpClientHandler()
            handler.CookieContainer <- container
            
            let request = new HttpRequestMessage()
            request.RequestUri <- Uri($"https://adventofcode.com/2024/day/{day}/input")
            
            use client = new HttpClient(handler)
            let! response = client.SendAsync(request) |> Async.AwaitTask
            response.EnsureSuccessStatusCode() |> ignore
            let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            File.WriteAllTextAsync(filename, content) |> Async.AwaitTask |> ignore
            
        let! puzzleInput = File.ReadAllLinesAsync(filename) |> Async.AwaitTask
        return puzzleInput
    }

[<EntryPoint>]
let main args =
    let day = 6
    printfn $"Advent of Code 2024 - Day {day}"
    printfn ""
    
    let puzzleInput = download day |> Async.RunSynchronously
    
    let solution1 = solve1 puzzleInput
    printfn "Part 1:"
    printfn $"{solution1}"
    printfn ""
    
    let solution2 = solve2 puzzleInput
    printfn "Part 2:"
    printfn $"{solution2}"
    0