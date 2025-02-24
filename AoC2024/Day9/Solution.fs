module AoC2024.Day9.Solution

let checksum (blocks: int list) =
    List.indexed blocks
    |> List.fold (fun (state : bigint) (i, b) -> (i * b |> bigint) + state) 0I

let rec defrag (blocks: int list) result =
    match blocks with
    | [] -> List.rev result
    | h :: t ->
        if h = -1 then
            let last = 0 // TODO
            defrag t (last :: result)
        else
            defrag t (h :: result)
    
let rec expand (blocks : int list) (id : int) isFile result =
    match blocks with
    | [] -> List.rev result
    | h :: t ->
        if isFile then
            let file = [ for i in 1..h -> id ]
            let newResult = List.append file result
            expand t (id + 1) (not isFile) newResult
        else
            let space = [ for i in 1..h -> -1 ]
            let newResult = List.append space result
            expand t id (not isFile) newResult

let solve1 (puzzleInput: string seq) =
    let word = puzzleInput |> Seq.head
    let blocks =
        word.ToCharArray()
        |> Array.map (fun x -> $"{x}" |> int)
        |> Array.toList
    let expanded = expand blocks 0 true []
    let result = checksum expanded
    result.ToString()
    
let solve2 (puzzleInput: string seq) =
    let result =
        0
    result.ToString()