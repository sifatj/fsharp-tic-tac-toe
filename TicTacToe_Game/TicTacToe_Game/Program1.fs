namespace TicTacToe_Game

open System
open FSharpPlus
open Helper


type Player =  
    |X 
    |O

type Cell = 
    |P of Player
    |Empty

    //
type Status = 
    |Won 
    |Draw
    |InProgress

let switchPlayer (p:Player)  = 
    match p with
    |X -> O
    |O -> X


let hasWon x (grid:Cell [,]) = 
    match grid with
    //horizontal check
    |arr when grid.[0, 0] = x && grid.[0,1] = x && grid.[0, 2] = x || grid.[1, 0] = x && grid.[1, 1] = x && grid.[1, 2] = x || grid.[2, 0] = x && grid.[2, 1] = x && grid.[2, 2] = x -> true
    //vertical checks
    |arr when grid.[0, 0] = x && grid.[1, 0] = x && grid.[2, 0] = x || grid.[0, 1] = x && grid.[1, 1] = x && grid.[2, 1] = x || grid.[0, 2] = x && grid.[1, 2] = x && grid.[2, 2] = x -> true
    //diagonal checks
    |arr when grid.[0, 0] = x && grid.[1, 1] = x && grid.[2, 2] = x || grid.[0, 2] = x && grid.[1, 1] = x && grid.[0, 2] = x -> true
    |_ -> false

let hasDrawn x (grid:Cell [,]) = 
    let available_cells = grid |> Seq.cast<Cell> |> Seq.filter (fun x -> x = Empty)
    Seq.isEmpty available_cells



let checkGridStatus x grid =
    if hasWon x grid then Won
    else if hasDrawn x grid then Draw
    else InProgress

let gameLostByPlayer player =
    printfn "Player %A has lost the game!!" player
    //keep track of score
    

let gameWonByPlayer player = 
    printfn "Player %A has won the game!!" player
    //keep track of score
        //Please insert your name
        //Save score?
    //Other player lost so call that
    gameLostByPlayer (switchPlayer player)


let checkGridStatus x grid =
    if hasWon x grid then Won
    else if hasDrawn x grid then Draw
    else InProgress

let getNewGrid (grid: Cell [,]) xpos ypos (token:Player) =
    grid |> Array2D.mapi (fun i1 i2 v -> if i1 = xpos && i2 = ypos then (P token) else v )
    // let newGrid = grid
    // newGrid.[xpos,ypos] <- P X
    //newgrid

let localGame() = 
    let grid = Array2D.create 3 3 Empty
    let rec updateGame grid (token:Player) = 
        printfn "Please input the row number"  //Mathc response with and format tupple and destroy
        let xpos = Console.ReadLine() |> int
        printfn "Please input the column number"
        let ypos = Console.ReadLine() |> int
        let newGrid = getNewGrid grid xpos ypos token
        let status = checkGridStatus (P token) newGrid
        match status with
        |Won -> Won
        |Draw -> Draw
        |InProgress -> updateGame newGrid (switchPlayer token)
    0

let rec onlineGame() = 
    printfn "You have these choices:
  
    1. Host game
    2. Join game

    "
    printf "Please select an  option: "
    match (System.Console.ReadLine() |> int) with
        |1 -> printfn "Host game"
        |2 -> printfn "Join game" //repeat
        |_ -> onlineGame()




let rec playMPGame() = 
    printfn "You have these choices:
  
    1. Local Game
    2. Online Game

    "
    printf "Please select an MP option: "
    match (System.Console.ReadLine() |> int) with
        |1 -> localGame()
        |2 -> onlineGame()
        |_ -> playMPGame()

let playAIGame = ()//printfn "Playing agaisnt AI"


let viewLeaderboard() = printfn "Viewing the leaderboard"
 


type Choice =
    |PlayAI of string
    |PlayMP of string
    |ViewLeaderboard of string


//Take command
//Parse it
//Return the result
namespace Dice
open Helper


let parseCommand x = 
    match x with
    |ParseRegex HelpPattern _->
        Help |> Result.Ok
    //Check which command and if good
    //Check if error

let runCommand x = ()//Check if good
    //Check if error

let rec gameLoop() =
    //

    printf "Please input the type of game you wish to play or view leaderboard (AIGame,LocalGame,MPGame,Leaderboard)"
    System.Console.ReadLine() |> (parseCommand >> runCommand)
    //match (System.Console.ReadLine()) with
    //    |"AIGame" -> playAIGame
    //    |_ -> gameLoop()
    0

[<EntryPoint>]
let main argv = 
    gameLoop()

    0 // return an integer exit code
