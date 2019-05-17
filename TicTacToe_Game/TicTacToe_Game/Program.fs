namespace TicTacToe_Game
module Program =
    open Helper
    open Error
    open FSharpPlus
    open Command

    let handleResult res = 
        match res with 
        |Result.Ok (msg)->
        printfn "%s" msg //show the result
        |Result.Error err->
                err |> Error.GetErrorMessage |> printfn "%s"

    let rec mainGame() = 
        printf "Please input the type of game you wish to play or view leaderboard (Type help for more details): "
        System.Console.ReadLine() |> (Command.parseCommand >=> runCommand) |> handleResult 
        mainGame()

    
    [<EntryPoint>]
    let main argv = //
        mainGame()