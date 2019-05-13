namespace TicTacToe_Game
module Command =
    open Helper
    open Error

    let PlayMessage = @"This is a help message"
    //add spaces
    let PlayAIPattern = @"^\s*[Pp][Ll][Aa][Yy][Aa][Ii]\s*$"
    let PlayLocalPattern = @"^\s*[Pp][Ll][Aa][Yy][Ll][Oo][Cc][Aa][Ll]\s*$"
    let PlayMPPattern = @"^\s*[Pp][Ll][Aa][Yy][Mm][Pp]\s*$"
    let ViewLeaderboardPattern = @"^\s*[Vv][Ii][Ee][Ww]\s*$" //Modify these to have space
    
    (*
    let runCommand x = 
        match x with
        |Result.Ok PlayAI -> printfn "Play AI boi"
        |Result.Ok PlayLocal -> playGameLocal
        |Result.Ok PlayMP -> printfn "PlayMP"
        |Result.Ok ViewLeaderboard -> printfn "Leaderboard"
        |Result.Error BlankError -> BlankError |> Error.GetErrorMessage |> printfn "%s"
        |Result.Error ParseError -> ParseError |> Error.GetErrorMessage |> printfn "%s"*)
    type Command = 
        |PlayAI
        |PlayLocal
        |PlayMP
        |ViewLeaderboard
        static member parseCommand x = 
            match x with
            |ParseRegex PlayAIPattern _->
                  PlayAI |> Result.Ok
            |ParseRegex PlayLocalPattern _->
                  PlayLocal |> Result.Ok
            |ParseRegex PlayMPPattern _->
                  PlayMP |> Result.Ok
            |ParseRegex ViewLeaderboardPattern _->
                  ViewLeaderboard |> Result.Ok
            |""-> BlankError |> Result.Error
            |_ -> ParseError x |> Result.Error


    let runCommand x  =
        match x with
        |PlayAI ->
            let result = "Result of Play ai" 
            Result.Ok (sprintf "%s" result)
        |PlayLocal ->
            let result = "Result of Play local" 
            Result.Ok (sprintf "%s" result)
        |PlayMP ->
            let result = "Result of Play mp" 
            Result.Ok (sprintf "%s" result)
        |ViewLeaderboard ->
            let result = "Result of view leaderboard" 
            Result.Ok (sprintf "%s" result)
        |_ ->
            Result.Error (RunCommandError) 

    