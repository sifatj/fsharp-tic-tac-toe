namespace TicTacToe_Game
module Helper =
    open System.Text.RegularExpressions
    open System

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
        |arr when grid.[0, 0] = x && grid.[1, 1] = x && grid.[2, 2] = x || grid.[0, 2] = x && grid.[1, 1] = x && grid.[2, 0] = x -> true
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

    let updateGrid (grid: Cell[,]) (pos:int*int) (token:Player) = 
        let newGrid = Array2D.copy grid 
        let xpos, ypos = pos
        newGrid.[xpos, ypos] <- P token
        newGrid

    let easyAI (available: seq<int*int>) = 
        let rand = new Random()
        let checkSeq = rand.Next(available |> Seq.length)
        let getRandVal = available |> Seq.item checkSeq
        getRandVal
(*
    let hardAI (available: seq<int*int>) = 
        if available <> []
        then //place token at each empty cell to check if it would result in a win
            if //there is a winning posibility for ai 
            then //add those indexes to seq and pick from that seq
            elif //check if the other player has two in a row. put blocking move in seq
            else //remaining available indexes in seq

 *)

    let block x (grid:Cell[,]) = 
        match x with
        | arr when grid.[0,0] = x && grid.[0,1] = x -> (true, (0,2))
        | arr when grid.[0,1] = x && grid.[0,2] = x -> (true, (0,0))
        | arr when grid.[1,0] = x && grid.[1,1] = x -> (true, (1,2))
        | arr when grid.[1,1] = x && grid.[1,2] = x -> (true, (1,0))
        | arr when grid.[2,0] = x && grid.[2,1] = x -> (true, (2,2))
        | arr when grid.[2,1] = x && grid.[2,2] = x -> (true, (2,0))
        | arr when grid.[0,0] = x && grid.[1,0] = x -> (true, (2,0))
        | arr when grid.[1,0] = x && grid.[2,0] = x -> (true, (0,0))
        | arr when grid.[0,1] = x && grid.[1,1] = x -> (true, (2,1))
        | arr when grid.[1,1] = x && grid.[2,1] = x -> (true, (0,1))
        | arr when grid.[0,2] = x && grid.[1,2] = x -> (true, (2,2))
        | arr when grid.[1,2] = x && grid.[2,2] = x -> (true, (0,2))
        | arr when grid.[0,0] = x && grid.[1,1] = x -> (true, (2,2))
        | arr when grid.[1,1] = x && grid.[2,2] = x -> (true, (0,0))
        | arr when grid.[0,2] = x && grid.[1,1] = x -> (true, (2,0))
        | arr when grid.[1,1] = x && grid.[2,0] = x -> (true, (0,2))
        | arr when grid.[2,0] = x && grid.[2,2] = x -> (true, (2,1))
        | arr when grid.[0,2] = x && grid.[2,2] = x -> (true, (1,2))
        | arr when grid.[0,1] = x && grid.[2,1] = x -> (true, (1,1))
        | arr when grid.[0,0] = x && grid.[2,0] = x -> (true, (1,0))
        | arr when grid.[0,0] = x && grid.[2,2] = x -> (true, (1,1))
        | arr when grid.[0,2] = x && grid.[2,0] = x -> (true, (1,1))
        | arr when grid.[1,0] = x && grid.[1,2] = x -> (true, (1,1))
        | arr when grid.[0,0] = x && grid.[0,2] = x -> (true, (0,1))
        |_ -> (false,(0,0))

    (*
    let blockMoves x grid =
        if fst (block x grid)  
        then 
            let move = snd
            let blockM = seq {(move)}
            
        else
            easyAI |> updateGrid grid x
        *)


    let aiMoves grid token = 
            let other = switchPlayer (token)
            if fst (block (P other) grid)  
            then 
                let move = snd (block (P other) grid)
                let seq = Seq.empty
                let bMoves = Seq.append seq [(move)] 
                bMoves
            else
                Seq.empty
        
            

    let anotherAI (available: seq<int*int>) grid token = //From a list of available moves (like coord (0,0) etc)
                                           //Return a sequence of moves which would result in a win
                                           //Ex: Go over every move in the list add it to the current grid 
                                           //If adding that move results in a win then filter 
                                           //

        //let moves = available |> Seq.map (fun tpl grid token-> updateGrid grid tpl token) |> Seq.filter (fun x -> checkGridStatus x grid = Won)


        let moves = available |> Seq.filter (fun (tpl)-> (updateGrid grid tpl token |> checkGridStatus (P token)) = Won)

            
        //let other = switchPlayer (token) 
        let aMoves = Seq.append moves (aiMoves grid token)
        printfn "winning moves!!!!!!!!!!!!!!!!!!!!!!!!!%A" moves
        printfn "Moves to block!!!!!!!!!!!!!!!!!!!!!! %A" (aiMoves grid token)
        
        if Seq.isEmpty aMoves 
        then 
            printfn "no moves to play"
            None
        else 
            printfn "i can play these moves %A" aMoves
            Some (Seq.head aMoves)

   
            
        