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

    type Status = 
        |Won 
        |Draw
        |Lost
        |InProgress



    let switchPlayer (p:Player)  = 
        match p with
        |X -> O
        |O -> X
    
    type playerData = {name:string;pToken:Player} //Player information

    let parseStringToSome x = 
        match x |> Int32.TryParse with    
            |(true,x) -> Some x
            |_  -> None

    let rec takePlayerCoord() = 
       printfn "Please input the row number"  //Dificulties - making it more compact ,  when last match expression is done that I get the values I need and not nothing
       let xpos = Console.ReadLine() |> parseStringToSome   
       printfn "Please input the column number" 
       let ypos = Console.ReadLine() |> parseStringToSome
       
       //let strContainsOnlyNumber (s:string) = s |> Seq.forall Char.IsDigit
       match xpos,ypos with  
            |Some xpos,Some ypos when ((0 <= xpos && xpos <= 2) && (0 <= ypos && ypos <= 2)) -> xpos,ypos  
            |Some xpos,Some ypos when ((xpos > 2 || xpos < 0) || (ypos > 2 || ypos < 0))  -> printfn "Row %i and Col %i must be between 0 and 2" xpos ypos;takePlayerCoord()
            |_ -> printfn "Row and Col are not integers,try again";takePlayerCoord()

    let rec createPlayer assignedToken =
        printfn "Player %s input your name" assignedToken
        let name = Console.ReadLine()

        if name = "" then printfn "The name can not be empty";createPlayer assignedToken
        else if name.Contains(" ") then printfn "The name should not have any spaces, just letters";createPlayer assignedToken
        else if name |> Seq.forall Char.IsDigit then printfn "The name is only characters";createPlayer assignedToken
        else if name |> Seq.forall (fun c -> System.Char.IsLetter(c)) then name
        else  printfn "The name is not available, make sure you only have letters";createPlayer assignedToken

    
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

    let updateGrid (grid: Cell [,])  (pos:int*int) (token:Player) =
        //grid |> Array2D.mapi (fun i1 i2 v -> if i1 = xpos && i2 = ypos then (P token) else v )
        let newGrid = Array2D.copy grid
        let xpos, ypos = pos
        newGrid.[xpos, ypos] <- P token
        newGrid


    let easyAI (available: seq<int*int>) = 
        let rand = new Random()
        let checkSeq = rand.Next(available |> Seq.length)
        let randVal = available |> Seq.item checkSeq
        randVal


    let rec takePositions (grid: Cell [,])=
        let (xpos,ypos) = takePlayerCoord()
        if grid.[xpos,ypos] = Empty then xpos,ypos
        else printfn "Slot already taken try again"
             takePositions grid
             
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
            
    let hardAI  (available: seq<int*int>) grid token = 

        let moves = available |> Seq.filter (fun (tpl)-> (updateGrid grid tpl token |> checkGridStatus (P token)) = Won)
 
   
        let aMoves = Seq.append moves (aiMoves grid token)
      
        
        if Seq.isEmpty aMoves 
        then 

            None
        else 
            Some (Seq.head aMoves)
 
    let hasLost (x:Cell) (grid:Cell [,]): bool = 
        let otherPlayer =  (match x with
                                |P X -> O
                                |P O -> X)
        hasWon (P otherPlayer) grid    
    
    type PlayerDetails = {
        PlayerName: string
        mutable Wins: int
        mutable Losses: int 
    }

    // Record containing array of player detail records
    type PlayerRecords = {
        PlayerData : PlayerDetails []
    }