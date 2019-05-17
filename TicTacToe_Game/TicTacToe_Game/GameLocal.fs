namespace TicTacToe_Game
module GameLocal =
    open Helper
    open System
    open ChangePlayerRecords
    let jsonFile  = "../../PlayerRecords.json"
    let rec runGame grid playerX playerO token =
            drawBoard grid
            printfn "Player %A's turn" token           
            let newGrid = updateGrid grid (takePositions grid) token
            let status = checkGridStatus (P token) newGrid
            match status with
                |Won -> 
                changePlayerRecords playerX.name jsonFile (checkGridStatus (P playerX.pToken) grid)
                changePlayerRecords playerO.name jsonFile (checkGridStatus (P playerO.pToken) grid)
                sprintf "Good job player %A you won" token //Saves scores
                |Draw -> sprintf "Last token inserted by player %A resulted in a draw" token
                |InProgress -> runGame newGrid playerX playerO (match token with
                                                                      |X -> O
                                                                      |O -> X) // -- Isssue was having indefitiend the type*)


    let playGameLocal() =
        //initiate the game
        let playerX = {name = createPlayer "X";pToken = X}
        let playerO = {name = createPlayer "O";pToken = O}
        let grid = Array2D.create 3 3 Empty 
            
        runGame grid playerX playerO X


