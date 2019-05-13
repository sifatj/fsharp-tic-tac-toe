namespace TicTacToe_Game
module GameLocal =
    open Helper
    open System

    let playGameLocal() =
        let grid = Array2D.create 3 3 Empty
        let rec updateGame grid (token:Player) = 
            printfn "Please input the row number"  //Mathc response with and format tupple and destroy
            let xpos = Console.ReadLine() |> int
            printfn "Please input the column number"
            let ypos = Console.ReadLine() |> int
            //Check if position can  be put
            let newGrid = getNewGrid grid xpos ypos token
            let status = checkGridStatus (P token) newGrid
            match status with
            |Won -> Won
            |Draw -> Draw
            |InProgress -> updateGame newGrid (switchPlayer token)
        updateGame grid X
    
        
        //create grid

