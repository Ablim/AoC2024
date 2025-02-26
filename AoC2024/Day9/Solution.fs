module AoC2024.Day9.Solution

let isSpace block =
    block < 0
    
let isFile block =
    block >= 0
             
let takeLastFileBlock (blocks: int[]) =
    let rec loop index =
        if index < 0 then
            (-1, -1)
        else if isFile blocks[index] then
            (index, blocks[index])
        else
            loop (index - 1)
    loop (blocks.Length - 1)

let checksum (blocks: int list) =
    List.indexed blocks
    |> List.fold (fun (state : bigint) (i, b) -> (i * b |> bigint) + state) 0I

let defrag (blocks: int list) =
    let mutable blocks' = List.toArray blocks
    for i in 0..(blocks'.Length - 1) do
        if isSpace blocks'[i] then
            let index, block = takeLastFileBlock blocks'
            
            if i < index then
                blocks'[i] <- block
                blocks'[index] <- -1
        
    Array.toList blocks'

let expand blocks =
    let rec loop blocks' id isFile result =
        match blocks' with
        | [] -> List.rev result
        | h :: t ->
            if isFile then
                let file = [ for _ in 1..h -> id ]
                let newResult = List.append file result
                loop t (id + 1) (not isFile) newResult
            else
                let space = [ for _ in 1..h -> -1 ]
                let newResult = List.append space result
                loop t id (not isFile) newResult
    loop blocks 0 true []

let solve1 (puzzleInput: string seq) =
    let word = puzzleInput |> Seq.head
    let blocks =
        word.ToCharArray()
        |> Array.map (fun x -> $"{x}" |> int)
        |> Array.toList
    
    let result =
        blocks
        |> expand
        |> defrag
        |> List.filter isFile
        |> checksum
        
    result.ToString()
    
let solve2 (puzzleInput: string seq) =
    let result =
        0
    result.ToString()