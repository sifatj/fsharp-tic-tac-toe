namespace TicTacToe_Game
module Leaderboard =
    open System.IO
    open FSharp.Json
    open Helper        

    // read from json file - perform file null check??? check json docs
    let playerFile = File.ReadAllText("../../PlayerRecords.json")


    // deserialize json into F# 
    let deserializedData = Json.deserialize<LeaderBoard> playerFile // json PlayerDetails
    
    // put deserialized data into array ( array of records ) 
    let playerRecordsArray = deserializedData.PlayerData |> Array.map (fun p -> p)

    
    // iterate through the array and print each records    
    printfn "Names Wins Losses";
    playerRecordsArray |> Array.iter (fun p -> printf "%s %0.0f %0.0f\n" p.PlayerName p.Wins p.Losses)
    
    // calculate win loss ratio
    let calculateWinLossRatio p = 
        (p.Wins / (p.Wins + p.Losses))
    // sort array in descending order based on player win loss ratio 
    let sortPlayerScore arr =  
        arr
        |> Array.sortByDescending (fun p -> calculateWinLossRatio p )

    let sortedArray = sortPlayerScore playerRecordsArray
    printfn "SORTED Array: \n  %A" sortedArray

    let viewLeaderboard () = "This is the leaderboard"