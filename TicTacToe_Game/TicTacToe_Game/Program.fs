namespace TicTacToe_Game
module Program =
    open FSharpPlus
    open Helper
    open Error
    open Command

    //open GameAI
    //open GameMP
    open GameLocal

    let PlayMessage = @"This is a help message"
    //add spaces
    let PlayAIPattern = @"^\s*[Pp][Ll][Aa][Yy][Aa][Ii]\s*$"
    let PlayLocalPattern = @"^\s*[Pp][Ll][Aa][Yy][Ll][Oo][Cc][Aa][Ll]\s*$"
    let PlayMPPattern = @"^\s*[Pp][Ll][Aa][Yy][Mm][Pp]\s*$"
    let ViewLeaderboardPattern = @"^\s*[Vv][Ii][Ee][Ww]\s*$" //Modify these to have space


    let runCommand x = 
        match x with
        |Result.Ok PlayAI -> printfn "Play AI boi"
        |Result.Ok PlayLocal -> playGameLocal
        |Result.Ok PlayMP -> printfn "PlayMP"
        |Result.Ok ViewLeaderboard -> printfn "Leaderboard"
        |Result.Error BlankError -> BlankError |> Error.GetErrorMessage |> printfn "%s"
        |Result.Error ParseError -> ParseError |> Error.GetErrorMessage |> printfn "%s"

    [<EntryPoint>]
    let main argv = //
        let rec gameMain = 
            System.Console.ReadLine() |> (Command.parseCommand >> runCommand)
 
        0 
