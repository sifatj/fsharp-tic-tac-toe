namespace TicTacToe_Game
module ChangePlayerRecords = 
    open System
    open System.IO
    open FSharp.Json
    open Helper

    //{p with p.Wins = p.Wins + 1}

    let playerFile = "../../PlayerRecords.json"
    let players = File.ReadAllText("../../PlayerRecords.json")

    let read file:string = File.ReadAllText(file)
    let saveToFile (filename:string) (leaderboardRec:PlayerRecords) = 
        File.WriteAllText(filename, (Json.serialize leaderboardRec))

    let newPlayerRecords (old:PlayerRecords) (playerRecord:PlayerDetails) = 
        let playerArray:PlayerDetails[] = [|playerRecord|]
        Array.append old.PlayerData playerArray

    let addToPlayerRecords (leaderBoard:PlayerRecords) (playerRecord: PlayerDetails) = 
        let playerRecordsArray = leaderBoard.PlayerData |> Array.map (fun x -> x)
        let playerIndex = Array.findIndex (fun (x:PlayerDetails) -> x.PlayerName = playerRecord.PlayerName) playerRecordsArray
        leaderBoard.PlayerData.[playerIndex].Wins <- leaderBoard.PlayerData.[playerIndex].Wins + playerRecord.Wins

        saveToFile playerFile leaderBoard

    let addLossPlayerRecords (leaderBoard:PlayerRecords) (playerRecord: PlayerDetails) = 
        let playerRecordsArray = leaderBoard.PlayerData |> Array.map (fun x -> x)
        let playerIndex = Array.findIndex (fun (x:PlayerDetails) -> x.PlayerName = playerRecord.PlayerName) playerRecordsArray
        leaderBoard.PlayerData.[playerIndex].Losses <- leaderBoard.PlayerData.[playerIndex].Losses + playerRecord.Losses
        saveToFile playerFile leaderBoard


    let appendPlayerRecords (leaderBoardRec:PlayerRecords) (playerRecord: PlayerDetails) = 
        let leaderboard = {PlayerData = (newPlayerRecords leaderBoardRec playerRecord)}
        let toJson = leaderboard |> Json.serialize
        File.WriteAllText(playerFile, toJson)

    let changePlayerRecords (name:string) (fileName:string) (gameStatus:Status)=
        let record = 
            if gameStatus = Won 
            then
                {PlayerName = name; Wins=1; Losses = 0;}
            else
                {PlayerName = name; Wins=0; Losses = 1;}
        let playerRecords = Json.deserialize<PlayerRecords> (read fileName)
        let array  = playerRecords.PlayerData
        let playerNames = Array.map(fun recElement -> recElement.PlayerName) array
        if Array.contains name playerNames && gameStatus = Won
            then addToPlayerRecords playerRecords record
        elif Array.contains name playerNames && gameStatus = Lost
            then addLossPlayerRecords playerRecords record
        else appendPlayerRecords playerRecords record