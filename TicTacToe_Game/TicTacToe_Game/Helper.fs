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
    
    let isEmpty newerGrid = newerGrid |> Seq.cast<Cell> |> Seq.mapi (fun i el -> (el, i)) |> Seq.filter (fun (el,i) -> el =  Empty)

    let ToTuple elIndexTup = elIndexTup |> Seq.map (fun (x, y) -> 
        match (x, y) with
        |(_, y) when y = 1 -> (0, 1)
        |(_, y) when y = 0 -> (0, 0)
        |(_, y) when y = 2 -> (0, 2)
        |(_, y) when y = 3 -> (1, 0)
        |(_, y) when y = 4 -> (1, 1)
        |(_, y) when y = 5 -> (1, 2)
        |(_, y) when y = 6 -> (2, 0)
        |(_, y) when y = 7 -> (2, 1)
        |(_, y) when y = 8 -> (2, 2)
        |_ -> (20, 20)
        )
    //let cells_tuple () = (Array2D.create 3 3 Empty |> Seq.cast<Cell> |> Seq.filter (fun (el i) -> el = Empty)
    //)
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

    let drawBoard (board:Cell [,]) = 
        printfn "%A | %A | %A " board.[0,0] board.[0,1] board.[0,2]
        printfn "%A | %A | %A " board.[1,0] board.[1,1] board.[1,2]
        printfn "%A | %A | %A " board.[2,0] board.[2,1] board.[2,2]
     //separate frunction that takes in a grid cell and converts it based on value X O or Empty


    let checkGridStatus x grid =
        if hasWon x grid then Won
        else if hasDrawn x grid then Draw
        else InProgress

    let getNewGrid (grid: Cell [,]) xpos ypos (token:Player) =
        grid |> Array2D.mapi (fun i1 i2 v -> if i1 = xpos && i2 = ypos then (P token) else v )