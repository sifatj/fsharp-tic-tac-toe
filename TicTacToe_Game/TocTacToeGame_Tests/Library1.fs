namespace TicTacToeGame_Tests
module TicTacToeGame =
    open NUnit.Framework
    open TicTacToe_Game.Helper
    open TicTacToe_Game.Command

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
