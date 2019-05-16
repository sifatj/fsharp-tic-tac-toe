namespace TicTacToe_Game
module EasyAIGame = 
    open Helper
    open System
    //using mutually recursive func (week 3 lab 1)
    let rec AIupdate grid (token:Player) =  
            let avail = isEmpty grid |> ToTuple |> easyAI
            let xpos,ypos = avail
            let newGrid = updateGrid grid (xpos, ypos) token
            let status = checkGridStatus (P token) newGrid
            match status with
            |Won -> "Computer won", token
            |Draw -> "It's a draw", token
            |InProgress -> updateGame newGrid (switchPlayer token)

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
                |Won ->  "Good job Player  you won",token
                |Draw -> "Nice try , it's a draw.",token
                |InProgress -> AIupdate newGrid (switchPlayer token)
            else 
                let r,s = "Position not available", token
                printfn "%s %A" r s
                updateGame grid token
                //"Position not available!!!!!!!", token
                
            

    let playEasyAI() =
        let grid = Array2D.create 3 3 Empty
        updateGame grid X

        
        
