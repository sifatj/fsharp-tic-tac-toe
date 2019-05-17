namespace TicTacToe_Game
module Leaderboard =
    open System.IO
    open FSharp.Json
    open Helper        

    // Relative filepath for json file
    let playerRecordsFilePath:string = "../../PlayerRecords.json"

    // Read from all lines from json file and store inside this value.
    // Catch exception when file not found.
    let playerRecordsFile:string = 
        try 
            File.ReadAllText(playerRecordsFilePath)
        with
            | :? System.IO.FileNotFoundException -> "Error: File not found."

    // Deserialize player json data
    let getPlayerRecords:PlayerRecords = Json.deserialize<PlayerRecords> playerRecordsFile 
    
    // Put deserialized data into array to create array of records 
    let playerRecordsArray = getPlayerRecords.PlayerData |> Array.map (fun p -> p)
    
    // Calculate score based off win loss ratio 
    let calculateWinLossRatio player = 
        (float(player.Wins) / float((player.Wins + player.Losses)))

    // Sort players in descending order based on player win loss ratio and return the top 3 players
    let sortByPlayerScores arr =  
        arr
        |> Array.sortByDescending (fun p -> calculateWinLossRatio p )
        |> Array.take 3
    
    // Array of top players sorted by their score
    let highestScoringPlayers = sortByPlayerScores playerRecordsArray
  
    // Concatenate strings from each element in array
    let concatStr strSequence = Array.fold (+) "" strSequence


    // Prints the leaderboard which contains player name, wins and losses for the top 3 players 
    let viewLeaderboard () = 
        printf  "----- Leaderboard -----" 
        printfn "Name   Wins  Losses"
        highestScoringPlayers |> Array.map (fun p -> sprintf "%s   %d    %d\n" p.PlayerName p.Wins p.Losses) |> concatStr
        
