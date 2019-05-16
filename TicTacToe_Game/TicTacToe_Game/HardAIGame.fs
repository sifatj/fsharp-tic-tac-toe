namespace TicTacToe_Game
module HardAIGame = 
    open Helper
    open System
    //using mutually recursive func (week 3 lab 1)
    let rec AIupdate grid (token:Player) (player:playerData)= 
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
                 sprintf "Computer %A won" token
                |Draw -> "It's a draw"
                |InProgress -> updateGame newGrid player
            else
                let newMove = easyAI avail
                printfn "use this move insteaddddddd %A" newMove
                let newGrid1 = updateGrid grid newMove token
                let status = checkGridStatus (P token) newGrid1
                match status with
                |Won -> 
                drawBoard newGrid1 
                sprintf "Computer %A won sorry" token
                |Draw -> sprintf "It's a draw sorry"
                |InProgress -> updateGame newGrid1 player

                //"Position not available", token
                //updateGrid grid ai token

                //AIupdate grid token
                

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
                drawBoard newGrid
                sprintf "Good job Player %A you won" token
                |Draw -> sprintf "Nice try %A is a draw" token
                |InProgress -> AIupdate newGrid (switchPlayer token) player
            else 
                let r,s = "Position not available", token
                printfn "%s %A" r s
                updateGame grid player
                //"Position not available!!!!!!!", token
                
            

    let playHardAI() =
        let grid = Array2D.create 3 3 Empty
        let playerX = {name = createPlayer "X";pToken = X}
        updateGame grid playerX
