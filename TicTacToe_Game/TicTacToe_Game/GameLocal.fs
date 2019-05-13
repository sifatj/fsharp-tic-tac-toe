namespace TicTacToe_Game
module GameLocal =
    open Helper
    open System

    let playGameLocal() =
        let grid = Array2D.create 3 3 Empty
        let rec updateGame grid (token:Player) = 
            drawBoard grid
            printfn "Player %A's turn" token
            printfn "Please input the row number"  //Mathc response with and format tupple and destroy
            let xpos = Console.ReadLine() |> int  //tryParse later on for better input
            printfn "Please input the column number"
            let ypos = Console.ReadLine() |> int
            let newGrid = getNewGrid grid xpos ypos token
            let status = checkGridStatus (P token) newGrid
            match status with
            |Won -> "Good job Player  you won",token
            |Draw -> "Nice try , it's a draw.",token
            |InProgress -> updateGame newGrid (switchPlayer token)
        updateGame grid X
    
        
        //create grid

