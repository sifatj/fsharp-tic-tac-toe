namespace TicTacToe_Game
module GameLocal =
    open Helper
    open System

    let updateGrid grid token = 
        //drawBoard grid
        //let token = player.pToken
        token |> getNewGrid grid (getInputs()) |> checkGridStatus (P token)
        
        (*let token = player.pToken
        printfn "Player %A's turn" token           
        let newGrid = getNewGrid grid (getInputs()) token
        let status = checkGridStatus (P token) newGrid *)

    let playGameLocal() =
        //initiate the game
        let playerX = {name = createPlayer "X";pToken = X}
        let playerO = {name = createPlayer "O";pToken = O}
        let grid = Array2D.create 3 3 Empty 

        //play the game
        let rec playGame grid player =
            drawBoard grid
            let token = player.pToken

            // do update, if status in progress remake with switch
            // if won return the mssage for won
            //return the message for draw


        (*
        let rec updateGrid grid (player:playerData) = 
            drawBoard grid
            let token = player.pToken
            printfn "Player %A's turn" token           
            let newGrid = getNewGrid grid (getInputs()) token
            let status = checkGridStatus (P token) newGrid
            match status with
                |Won -> "Good job Player  you won",token
                |Draw -> "Nice try , it's a draw.",token
                |InProgress -> updateGame newGrid (match token with
                                                         |X -> playerO
                                                         |O -> playerX) // -- Isssue was having indefitiend the type*)
        updateGame grid playerX


