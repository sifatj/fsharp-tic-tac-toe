namespace TicTacToe_Game
module Command =
    open Helper
    open Error
    open GameLocal
    open EasyAIGame
    open HardAIGame

    open Leaderboard

    let HelpMessage = @"You have the following commands:
     |PlayAIHard
     |PlayAIEasy
     |PlayLocal
     |PlayMP
     |ViewLeaderboard"
    //add spaces
    let HelpPattern = @"^\s*[ .-]?[Hh][Ee][Ll][Pp][ .-]?\s*$"
    let PlayAIHardPattern = @"^\s*[Pp][Ll][Aa][Yy][ .-]?[Aa][Ii][ .-]?[Hh][Aa][Rr][Dd]\s*$"
    let PlayAIEasyPattern = @"^\s*[Pp][Ll][Aa][Yy][ .-]?[Aa][Ii][ .-]?[Ee][Aa][Ss][Yy]\s*$"
    let PlayLocalPattern = @"^\s*[Pp][Ll][Aa][Yy][ .-]?[Ll][Oo][Cc][Aa][Ll][ .-]?[Gg][Aa][Mm][Ee]\s*$"
    let PlayMPPattern = @"^\s*[Pp][Ll][Aa][Yy][ .-]?[Mm][Pp][ .-]?[Gg][Aa][Mm][Ee]\s*$"
    let ViewLeaderboardPattern = @"^\s*[Vv][Ii][Ee][Ww][ .-]?[Ll][Ee][Aa][Dd][Ee][Rr][Bb][Oo][Aa][Rr][Dd]\s*$" //Modify these to have space
   
    type Command = 
        |PlayAIHard
        |PlayAIEasy
        |PlayLocal
        |PlayMP
        |ViewLeaderboard
        |Help
        static member parseCommand x = 
            match x with
            |ParseRegex PlayAIHardPattern _->
                  PlayAIHard |> Result.Ok
            |ParseRegex PlayAIEasyPattern _->
                  PlayAIEasy |> Result.Ok
            |ParseRegex PlayLocalPattern _->
                  PlayLocal |> Result.Ok
            |ParseRegex PlayMPPattern _->
                  PlayMP |> Result.Ok
            |ParseRegex ViewLeaderboardPattern _->
                  ViewLeaderboard |> Result.Ok
            |ParseRegex HelpPattern _->
                  Help |> Result.Ok
            |""-> BlankError |> Result.Error
            |_ -> ParseError x |> Result.Error


    let runCommand x  =
        match x with
        |PlayAIEasy ->
            let result = playEasyAI() 
            Result.Ok (sprintf "%s" result)
        |PlayAIHard ->
            let result = playHardAI()
            Result.Ok (sprintf "%s" result)
        |PlayLocal ->
            let result = playGameLocal() 
            Result.Ok (sprintf "%s" result)
        |PlayMP ->
            let result = "Result of Play mp" 
            Result.Ok (sprintf "%s" result)
        |ViewLeaderboard ->
            let result = viewLeaderboard() 
            Result.Ok (sprintf "%s" result)
        |Help ->
            Result.Ok (HelpMessage)
        |_ ->
            Result.Error (RunCommandError) 

    