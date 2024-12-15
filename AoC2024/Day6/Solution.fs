module AoC2024.Day6.Solution

type Direction =
    | Up = 0
    | Down = 1
    | Right = 2
    | Left = 3

let rec walk (pos: int * int) (dir: Direction) (map: char[][]) (path: (int * int) list) =
    let height = map.Length
    let width = map[0].Length
    let row, col = pos
    
    match dir with
    | Direction.Up ->
        let newRow = row - 1
        if newRow < 0 then
            path
        else
            if map[newRow][col] = '#' then
                walk pos Direction.Right map (pos :: path)
            else
                walk (newRow, col) dir map ((newRow, col) :: path)
    | Direction.Down ->
        let newRow = row + 1
        if newRow >= height then
            path
        else
            if map[newRow][col] = '#' then
                walk pos Direction.Left map (pos :: path)
            else
                walk (newRow, col) dir map ((newRow, col) :: path)
    | Direction.Right ->
        let newCol = col + 1
        if newCol >= width then
            path
        else
            if map[row][newCol] = '#' then
                walk pos Direction.Down map (pos :: path)
            else
                walk (row, newCol) dir map ((row, newCol) :: path)
    | Direction.Left ->
        let newCol = col - 1
        if newCol < 0 then
            path
        else
            if map[row][newCol] = '#' then
                walk pos Direction.Up map (pos :: path)
            else
                walk (row, newCol) dir map ((row, newCol) :: path)
    | _ -> System.ArgumentOutOfRangeException() |> raise

let canInsert (pos: int * int) (dir: Direction) (path: (int * int * Direction) list) (map: char[][]) =
    let lastRow = map.Length - 1
    let lastCol = map[0].Length - 1
    let row, col = pos
    
    match dir with
    | Direction.Up ->
        let overlaps =
            seq {
                for r in 0..row do
                    let candidate = (r, col, dir)
                    if List.contains candidate path then candidate
            }
            |> Seq.length
        
        if overlaps > 0 then
            Some (row, col - 1)
        else
            Option.None
    | Direction.Down ->
        let overlaps =
            seq {
                for r in row..lastRow do
                    let candidate = (r, col, dir)
                    if List.contains candidate path then candidate
            }
            |> Seq.length
        
        if overlaps > 0 then
            Some (row, col + 1)
        else
            Option.None
    | Direction.Right ->
        let overlaps =
            seq {
                for c in col..lastCol do
                    let candidate = (row, c, dir)
                    if List.contains candidate path then candidate
            }
            |> Seq.length
        
        if overlaps > 0 then
            Some (row - 1, col)
        else
            Option.None
    | Direction.Left ->
        let overlaps =
            seq {
                for c in 0..col do
                    let candidate = (row, c, dir)
                    if List.contains candidate path then candidate
            }
            |> Seq.length
        
        if overlaps > 0 then
            Some (row + 1, col)
        else
            Option.None
    | _ -> System.ArgumentOutOfRangeException() |> raise

let rec walk2 (pos: int * int) (dir: Direction) (map: char[][]) (path: (int * int * Direction) list) (stops: (int * int) list) =
    let height = map.Length
    let width = map[0].Length
    let row, col = pos
    
    match dir with
    | Direction.Up ->
        let newRow = row - 1
        if newRow < 0 then
            stops
        else
            if map[newRow][col] = '#' then
                walk2 pos Direction.Right map ((row, col, Direction.Right) :: path) stops
            else
                let stop = canInsert pos Direction.Right path map
                let newStops = if stop.IsSome then (stop.Value :: stops) else stops
                walk2 (newRow, col) dir map ((newRow, col, dir) :: path) newStops
    | Direction.Down ->
        let newRow = row + 1
        if newRow >= height then
            stops
        else
            if map[newRow][col] = '#' then
                walk2 pos Direction.Left map ((row, col, Direction.Left) :: path) stops
            else
                let stop = canInsert pos Direction.Left path map
                let newStops = if stop.IsSome then (stop.Value :: stops) else stops
                walk2 (newRow, col) dir map ((newRow, col, dir) :: path) newStops
    | Direction.Right ->
        let newCol = col + 1
        if newCol >= width then
            stops
        else
            if map[row][newCol] = '#' then
                walk2 pos Direction.Down map ((row, col, Direction.Down) :: path) stops
            else
                let stop = canInsert pos Direction.Down path map
                let newStops = if stop.IsSome then (stop.Value :: stops) else stops
                walk2 (row, newCol) dir map ((row, newCol, dir) :: path) newStops
    | Direction.Left ->
        let newCol = col - 1
        if newCol < 0 then
            stops
        else
            if map[row][newCol] = '#' then
                walk2 pos Direction.Up map ((row, col, Direction.Up) :: path) stops
            else
                let stop = canInsert pos Direction.Up path map
                let newStops = if stop.IsSome then (stop.Value :: stops) else stops
                walk2 (row, newCol) dir map ((row, newCol, dir) :: path) newStops
    | _ -> System.ArgumentOutOfRangeException() |> raise

let solve1 (puzzleInput: string seq) =
    let map =
        puzzleInput
        |> Seq.map (fun x -> x.ToCharArray())
        |> Seq.toArray
    let start =
        seq {
            for row in 0..map.Length - 1 do
                for col in 0..map[0].Length - 1 do
                    if map[row][col] = '^' then
                        (row, col)
        }
        |> Seq.head
    let result =
        walk start Direction.Up map [ start ]
        |> List.distinct
        |> List.length
    result.ToString()
    
let solve2 (puzzleInput: string seq) =
    let map =
        puzzleInput
        |> Seq.map (fun x -> x.ToCharArray())
        |> Seq.toArray
    let startRow, startCol =
        seq {
            for row in 0..map.Length - 1 do
                for col in 0..map[0].Length - 1 do
                    if map[row][col] = '^' then
                        (row, col)
        }
        |> Seq.head
    let result =
        walk2 (startRow, startCol) Direction.Up map [ (startRow, startCol, Direction.Up) ] []
        |> List.distinct
        |> List.length
    result.ToString()