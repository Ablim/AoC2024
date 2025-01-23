module AoC2024.Day7.Solution

let rec calculate (answer: bigint) (maybeAnswer: bigint) numbers =
    match numbers with
    | [] -> answer = maybeAnswer
    | h :: t ->
        if maybeAnswer > answer then
            false
        else
            calculate answer (h + maybeAnswer) t || calculate answer (h * maybeAnswer) t
            
let rec calculate2 (answer: bigint) (maybeAnswer: bigint) numbers =
    match numbers with
    | [] -> answer = maybeAnswer
    | h :: t ->
        if maybeAnswer > answer then
            false
        else
            calculate2 answer (maybeAnswer + h) t
            || calculate2 answer (maybeAnswer * h) t
            || calculate2 answer ($"{maybeAnswer}{h}" |> bigint.Parse) t

let solve1 (puzzleInput: string seq) =
    let answers =
        puzzleInput
        |> Seq.map (fun x -> x.Split(':'))
        |> Seq.map (fun x -> x[0] |> bigint.Parse)
        |> Seq.toArray
    let terms =
        puzzleInput
        |> Seq.map (fun x -> x.Split(' '))
        |> Seq.map (fun x -> x |> Array.toList |> List.tail)
        |> Seq.map (fun x -> x |> List.map (fun y -> y |> bigint.Parse))
        |> Seq.toArray
        
    let result =
        seq {
            for i in 0..answers.Length - 1 do
                if calculate answers[i] 0I terms[i] then
                    answers[i]
        }
        |> Seq.sum
    result.ToString()
    
let solve2 (puzzleInput: string seq) =
    let answers =
        puzzleInput
        |> Seq.map (fun x -> x.Split(':'))
        |> Seq.map (fun x -> x[0] |> bigint.Parse)
        |> Seq.toArray
    let terms =
        puzzleInput
        |> Seq.map (fun x -> x.Split(' '))
        |> Seq.map (fun x -> x |> Array.toList |> List.tail)
        |> Seq.map (fun x -> x |> List.map (fun y -> y |> bigint.Parse))
        |> Seq.toArray
        
    let result =
        seq {
            for i in 0..answers.Length - 1 do
                if calculate2 answers[i] terms[i].Head terms[i].Tail then
                    answers[i]
        }
        |> Seq.sum
    result.ToString()