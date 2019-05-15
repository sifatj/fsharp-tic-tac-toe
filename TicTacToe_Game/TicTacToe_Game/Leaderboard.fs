namespace TicTacToe_Game
module Leaderboard =
    open System.IO
    open FSharp.Json
    open Helper        

    // read from json file
    let playerFile = File.ReadAllText("../../PlayerRecords.json")
    //printfn "%A" playerFile
    printfn "----- playerFile -----"
    printfn "%s" playerFile

    // deserialize json into F# 
    let deserializedData = Json.deserialize<LeaderBoard> playerFile // json PlayerDetails
    printfn "----- deserialzedData -----"
    printfn "%A" deserializedData
    
    // put deserialized data into array ( array of records ) 
    let playerRecordsArray = deserializedData.PlayerData |> Array.map (fun p -> p)
    printfn "----- playerRecordsArray -----"
    printfn "%A" playerRecordsArray 
    
    // iterate through the array and print each records 
    printfn " ---- table ---- "
    //playerRecordsArray |> Array.iter (fun p -> printf "Name: %s, Wins: %0.0f, Losses: %0.0f\n" p.PlayerName p.Wins p.Losses)  
    
    printfn "Names Wins Losses";
    playerRecordsArray |> Array.iter (fun p -> printf "%s %0.0f %0.0f\n" p.PlayerName p.Wins p.Losses)
    // get score ratio
    let calculateWinLossRatio p = 
        (p.Wins / (p.Wins + p.Losses))
    // sort list in descending order based on player win ratio 
    let sortPlayerScore arr =  
        arr
        |> Array.sortByDescending (fun p -> calculateWinLossRatio p )

    let sortedArray = sortPlayerScore playerRecordsArray
    printfn "SORTED Array: \n  %A" sortedArray

    let viewLeaderboard () = "This is the leaderboard"