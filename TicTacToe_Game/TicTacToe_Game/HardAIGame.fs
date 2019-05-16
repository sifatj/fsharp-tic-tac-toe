namespace TicTacToe_Game
module HardAIGame = 
    open Helper
    open System
    //using mutually recursive func (week 3 lab 1)
    let rec AIupdate grid (token:Player) = 
            let avail = isEmpty grid |> ToTuple 
            let result = anotherAI avail grid token
            let ai = avail |> easyAI
            let move = 
                match result with 
                |None -> 
                 printfn"no moves so random pick"
                 ai
                |Some (xpos,ypos) -> (xpos,ypos)
            //let avail = isEmpty grid |> ToTuple |> easyAI
            let xpos,ypos = move
            //let newGrid = updateGrid grid (xpos, ypos) token
            //let status = checkGridStatus (P token) newGrid
            //checks if move is available 
            if Seq.contains move avail
            then
                let newGrid = updateGrid grid (xpos, ypos) token
                let status = checkGridStatus (P token) newGrid
                match status with
                |Won -> 
                 drawBoard newGrid
                 "Computer won", token
                |Draw -> "It's a draw", token
                |InProgress -> updateGame newGrid (switchPlayer token)
            else
                let newMove = easyAI avail
                printfn "use this move insteaddddddd %A" newMove
                let newGrid1 = updateGrid grid newMove token
                let status = checkGridStatus (P token) newGrid1
                match status with
                |Won -> 
                drawBoard newGrid1 
                "Computer won sorry", token
                |Draw -> "It's a draw sorry", token
                |InProgress -> updateGame newGrid1 (switchPlayer token)

                //"Position not available", token
                //updateGrid grid ai token

                //AIupdate grid token
                

    and updateGame grid (token:Player) = 
            drawBoard grid
            printfn "Player %A's turn" token
            printfn "Please input the row number"  //Mathc response with and format tupple and destroy
            let xpos = Console.ReadLine() |> int  //tryParse later on for better input
            printfn "Please input the column number"
            let ypos = Console.ReadLine() |> int
            let position = xpos, ypos
            let avail = isEmpty grid |> ToTuple
            if Seq.contains position avail
            then 
                let newGrid = updateGrid grid (xpos, ypos) token
                let status = checkGridStatus (P token) newGrid
                match status with
                |Won -> 
                drawBoard newGrid
                "Good job Player  you won",token
                |Draw -> "Nice try , it's a draw.",token
                |InProgress -> AIupdate newGrid (switchPlayer token)
            else 
                let r,s = "Position not available", token
                printfn "%s %A" r s
                updateGame grid token
                //"Position not available!!!!!!!", token
                
            

    let playHardAI() =
        let grid = Array2D.create 3 3 Empty
        updateGame grid X

