namespace TicTacToe_Game
module EasyAIGame = 
    open Helper
    open System

    let rec AIupdate grid (token:Player) (player:playerData)=  
            let avail = isEmpty grid |> ToTuple |> easyAI
            let xpos,ypos = avail
            let newGrid = updateGrid grid (xpos, ypos) token
            let status = checkGridStatus (P token) newGrid
            match status with
            |Won -> sprintf "Computer %A won" token
            |Draw -> sprintf "It's a draw"
            |InProgress -> updateGame newGrid player

    and updateGame grid (player:playerData) = 
            drawBoard grid
            let token = player.pToken
            printfn "Player %A's turn" token
            let position = takePositions grid
            let avail = isEmpty grid |> ToTuple
            if Seq.contains position avail
            then 
                let newGrid = updateGrid grid (position) token
                let status = checkGridStatus (P token) newGrid
                match status with
                |Won ->  
                //Save score  player is a record boi
                sprintf "Good job Player %A you won" token
                |Draw -> sprintf "Nice try %A is a draw" token
                |InProgress -> AIupdate newGrid (switchPlayer token) player
            else 
                let rs = sprintf "Position for player %A not available" token
                printfn "%s" rs
                updateGame grid player

            

    let playEasyAI() =
        let grid = Array2D.create 3 3 Empty
        let playerX = {name = createPlayer "X";pToken = X}
        updateGame grid playerX

        
        