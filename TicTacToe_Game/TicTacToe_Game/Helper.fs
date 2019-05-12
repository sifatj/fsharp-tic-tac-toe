namespace TicTacToe_Game
module Helper =
    open System.Text.RegularExpressions

    let (|ParseRegex|_|) regex str =
       let m = Regex(regex).Match(str)
       if m.Success
       then Some (List.tail [ for x in m.Groups -> x.Value ])
       else None

    type Player =  
    |X 
    |O

    type Cell = 
        |P of Player
        |Empty

        //
    type Status = 
        |Won 
        |Draw
        |InProgress

    let switchPlayer (p:Player)  = 
        match p with
        |X -> O
        |O -> X

    let hasWon x (grid:Cell [,]) = 
        match grid with
        //horizontal check
        |arr when grid.[0, 0] = x && grid.[0,1] = x && grid.[0, 2] = x || grid.[1, 0] = x && grid.[1, 1] = x && grid.[1, 2] = x || grid.[2, 0] = x && grid.[2, 1] = x && grid.[2, 2] = x -> true
        //vertical checks
        |arr when grid.[0, 0] = x && grid.[1, 0] = x && grid.[2, 0] = x || grid.[0, 1] = x && grid.[1, 1] = x && grid.[2, 1] = x || grid.[0, 2] = x && grid.[1, 2] = x && grid.[2, 2] = x -> true
        //diagonal checks
        |arr when grid.[0, 0] = x && grid.[1, 1] = x && grid.[2, 2] = x || grid.[0, 2] = x && grid.[1, 1] = x && grid.[0, 2] = x -> true
        |_ -> false

    let hasDrawn x (grid:Cell [,]) = 
        let available_cells = grid |> Seq.cast<Cell> |> Seq.filter (fun x -> x = Empty)
        Seq.isEmpty available_cells



    let checkGridStatus x grid =
        if hasWon x grid then Won
        else if hasDrawn x grid then Draw
        else InProgress

    let getNewGrid (grid: Cell [,]) xpos ypos (token:Player) =
        grid |> Array2D.mapi (fun i1 i2 v -> if i1 = xpos && i2 = ypos then (P token) else v )