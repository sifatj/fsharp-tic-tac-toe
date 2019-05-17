namespace TicTacToeGame_Tests
module TicTacToeGame =
    open NUnit.Framework
    open TicTacToe_Game.Helper
    open TicTacToe_Game.Command
    open TicTacToe_Game.EasyAIGame
    open TicTacToe_Game.HardAIGame
    open FsCheck

    [<Test>]
    let ``Check parsing a string to integer`` () =
        Assert.AreEqual(Some 1,parseStringToSome "1")


    [<Test>]
    let ``Check parsing a string to integer is wrong`` () =
        Assert.AreNotEqual(Some 1,parseStringToSome "one")

    [<Test>]
    let ``Check grid status is won`` () =
        let grid = 
            array2D [
                [ P X; P X; P X ]
                [ Empty; Empty; Empty ]
                [ Empty; Empty; Empty ]
            ]
        Assert.AreEqual(Won,checkGridStatus (P X) grid)

    [<Test>]
    let ``Check grid status is draw`` () =
        let grid = 
            array2D [
                [ P X; P X; P O ]
                [ P O; P O; P X ]
                [ P X; P X; P O ]
            ]
        Assert.AreEqual(Draw,checkGridStatus (P X) grid)
    
    [<Test>]
    let ``Check grid status is in Progress`` () =
        let grid = 
            array2D [
                [ P X; P X; P O ]
                [ Empty; Empty; P X ]
                [ P X; P X; Empty ]
            ]
        Assert.AreEqual(InProgress,checkGridStatus (P X) grid)

    [<Test>]
    let ``Check that a new grid is returned`` () =
        let grid = 
            array2D [
                [ Empty; P X; P O ]
                [ Empty; Empty; P X ]
                [ P X; P X; Empty ]
            ]

        let newGrid = 
            array2D [
                [ P X; P X; P O ]
                [ Empty; Empty; P X ]
                [ P X; P X; Empty ]
            ]
        Assert.AreEqual(newGrid,updateGrid grid (0,0) X )




    (* Leaderboard.fs file tests *)

    // Dummy Record 
    type TestPlayerRecord = {
        PlayerName: string
        Wins: int
        Losses: int 
    }

    [<Test>]
    let ``Calculate win loss ratio correctly`` () =
        let playerRecord:TestPlayerRecord = {
            PlayerName = "sifat"
            Wins = 1
            Losses = 1 
        }
        let calculateWinLossRatio player = 
            (float player.Wins / float (player.Wins + player.Losses))
        let expected = float (1) / float (1+1)
        let actual = calculateWinLossRatio playerRecord
        Assert.AreEqual(expected, actual)

    [<Test>]
    let ``Calculate win loss ratio incorrectly`` () =
        let playerRecord:TestPlayerRecord = {
            PlayerName = "sifat"
            Wins = 1
            Losses = 2 
        }
        let calculateWinLossRatio player = 
            (float player.Wins / float (player.Wins + player.Losses))
        let expected = float (1) / float (1 + 4)
        let actual = calculateWinLossRatio playerRecord
        Assert.AreNotEqual(expected, actual)
    
    [<Test>]
    let ``Concat array of strings`` () =
        let concatStr sequence = Array.fold (+) "" sequence
        let a = "a b c"
        StringAssert.Contains(a, concatStr [|"a b c"|]  )
    (* Leaderboard.fs file tests end*)

    let grid = 
        array2D [
            [ Empty; P X; Empty ]
            [ Empty; Empty; Empty ]
            [ Empty; Empty; Empty ]
        ]

    let updatedGrid = 
        array2D [
            [ P O; P X; Empty ]
            [ Empty; Empty; Empty ]
            [ Empty; Empty; Empty ]
        ]

    [<Test>]
    let ``add token using updateGrid`` () = 
        Assert.AreEqual(updatedGrid, updateGrid grid (0,0) O)

    [<Test>]
    let ``find all Empty cells in grid`` () =
        let result = seq [(Empty, 0); (Empty, 2); (Empty, 3); (Empty, 4); (Empty, 5); (Empty, 6); (Empty, 7); (Empty, 8);]
        Assert.AreEqual (result, isEmpty grid)

    [<Test>]
    let ``pick random value from sequence of available cells`` () = 
        let availableCells = seq [(0, 0); (0, 2); (1, 0); (1, 1); (1, 2); (2, 0); (2, 1); (2, 2);]
        let prop_tupleRange (x, y) =
            (x>0 && x<=2 && y>0 && y<=2) 
        let prop_ContainedInSequence xs =
            easyAI xs |> fun cell -> Seq.contains cell xs
        //Check.QuickThrowOnFailure prop_ContainedInSequence
        //this only tests one seq
        Assert.IsTrue (prop_ContainedInSequence availableCells)


    let blockGrid = 
        array2D [
            [ P O; P X; Empty ]
            [ Empty; P X; Empty ]
            [ Empty; Empty; Empty ]
        ]

    [<Test>]
    let ``pick right move to block player from win`` () =
        Assert.AreEqual((true, (2,1)), block (P X) blockGrid)
   
