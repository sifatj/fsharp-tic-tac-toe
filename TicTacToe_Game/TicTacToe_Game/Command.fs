namespace TicTacToe_Game
module Command =
    open Helper
    open Error
    open GameLocal
    open Leaderboard

    let PlayMessage = @"This is a help message"
    //add spaces
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
        static member parseCommand x = 
            match x with
            |ParseRegex PlayAIHardPattern _->
                  PlayAIHard |> Result.Ok
            |ParseRegex PlayAIHardPattern _->
                  PlayAIEasy |> Result.Ok
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
        |PlayAIEasy ->
            let result = "Result of Play ai easy" 
            Result.Ok (sprintf "%s" result)
        |PlayAIHard ->
            let result = "Result of Play ai hard" 
            Result.Ok (sprintf "%s" result)
        |PlayLocal ->
            let result = playGameLocal() 
            let (str,token) = result
            Result.Ok (sprintf "%s %A" str token)
        |PlayMP ->
            let result = "Result of Play mp" 
            Result.Ok (sprintf "%s" result)
        |ViewLeaderboard ->
            let result = viewLeaderboard() 
            Result.Ok (sprintf "%s" result)
        |_ ->
            Result.Error (RunCommandError) 

    