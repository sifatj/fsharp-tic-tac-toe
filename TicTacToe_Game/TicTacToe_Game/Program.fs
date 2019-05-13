namespace TicTacToe_Game
module Program =
    open Helper
    open Error
    open FSharpPlus
    open Command

    //open GameAI
    //open GameMP//
    open GameLocal

    let PlayMessage = @"This is a help message"
    //add spaces
    let PlayAIPattern = @"^\s*[Pp][Ll][Aa][Yy][Aa][Ii]\s*$"
    let PlayLocalPattern = @"^\s*[Pp][Ll][Aa][Yy][Ll][Oo][Cc][Aa][Ll]\s*$"
    let PlayMPPattern = @"^\s*[Pp][Ll][Aa][Yy][Mm][Pp]\s*$"
    let ViewLeaderboardPattern = @"^\s*[Vv][Ii][Ee][Ww]\s*$" //Modify these to have space


    
    let handleResult res = 
        match res with 
        |Result.Ok (msg)->
        printfn "%s" msg //show the result
        |Result.Error err->
                err |> Error.GetErrorMessage |> printfn "%s"

    let rec mainGame() = 
        printf "Please input the type of game you wish to play or view leaderboard (AIGame,LocalGame,MPGame,Leaderboard): "
        System.Console.ReadLine() |> (Command.parseCommand >=> runCommand) |> handleResult 
        mainGame()

                    


       
    [<EntryPoint>]
    let main argv = //
        mainGame()