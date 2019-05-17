namespace TestingAI
module AITestsNUnit = 
    open TicTacToe_Game.Helper
    open TicTacToe_Game.EasyAIGame
    open TicTacToe_Game.HardAIGame
    open NUnit.Framework
    open FsCheck
    open System

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