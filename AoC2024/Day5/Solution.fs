module AoC2024.Day5.Solution

let rec check (pages: int list) (map: Map<int, int list>) =
    match pages with
    | [] -> true
    | h :: t ->
        let wrongOrder =
            map
            |> Map.filter (fun k _ -> List.contains k t)
            |> Map.values
            |> List.concat
            |> List.contains h
        if wrongOrder then
            false
        else
            check t map

let rec fix (pages: int list) (map: Map<int, int list>) (sorted: int list) =
    match pages with
    | [] -> sorted
    | h :: t ->
        let indices =
            seq {
                for i in 0..t.Length - 1 do
                    let values = if map.ContainsKey t[i] then map[t[i]] else []
                    if List.contains h values then
                        i
            }
        
        if Seq.isEmpty indices then
            fix t map (h :: sorted)
        else
            let index = Seq.head indices
            let newTail = List.insertAt (index + 1) h t
            fix newTail map sorted

let solve1 (puzzleInput: string seq) =
    let rules =
        puzzleInput
        |> Seq.filter (fun x -> x.Contains("|"))
        |> Seq.map (fun x -> x.Split('|'))
        |> Seq.map (fun x -> (x[0] |> int, x[1] |> int))
        |> Seq.groupBy fst
        |> Seq.map (fun (k, v) -> (k, v |> Seq.map snd |> Seq.toList))
        |> Map
    let pages =
        puzzleInput
        |> Seq.filter (fun x -> x.Contains(","))
        |> Seq.map (fun x -> x.Split(',') |> Array.map (fun y -> y |> int) |> Array.toList)
    
    let result =
        pages
        |> Seq.filter (fun x -> check x rules)
        |> Seq.map (fun x -> x |> List.toArray)
        |> Seq.map (fun x -> x[x.Length / 2])
        |> Seq.sum
    result.ToString()
    
let solve2 (puzzleInput: string seq) =
    let rules =
        puzzleInput
        |> Seq.filter (fun x -> x.Contains("|"))
        |> Seq.map (fun x -> x.Split('|'))
        |> Seq.map (fun x -> (x[0] |> int, x[1] |> int))
        |> Seq.groupBy fst
        |> Seq.map (fun (k, v) -> (k, v |> Seq.map snd |> Seq.toList))
        |> Map
    let pages =
        puzzleInput
        |> Seq.filter (fun x -> x.Contains(","))
        |> Seq.map (fun x -> x.Split(',') |> Array.map (fun y -> y |> int) |> Array.toList)
    
    let result =
        pages
        |> Seq.filter (fun x -> check x rules |> not)
        |> Seq.map (fun x -> fix x rules [])
        |> Seq.map (fun x -> x |> List.toArray)
        |> Seq.map (fun x -> x[x.Length / 2])
        |> Seq.sum
    result.ToString()