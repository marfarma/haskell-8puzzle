module Main where
    import qualified Graphics.HGL as HGL
    import qualified PGraphics as G
    import qualified PAlgorithm as A

    main = do
        let (myInitP, myBoard) = testBoard
        HGL.runGraphics $
            do
            w <- HGL.openWindow "8puzzle" (400,400)
            font <- HGL.createFont (50,50)  (pi/4)  False False "helvetica"
            G.drawBoard w font myBoard

            --Solve the 8puzzle
            let sol = A.solve myBoard myInitP
            putStrLn $ "Solved in " ++ (show (length sol)) ++ " shifts:"
            putStrLn $ show $ sol

            A.showSequence myInitP myBoard sol

            HGL.wGetChar w
            HGL.closeWindow w


    --Test boards for the program
    testBoard :: (A.Pos, A.Board) --The optimal solution for this board has 28 shifts
    testBoard = foldl ff (0,A.initBoard) [A.UpS, A.UpS, A.LeftS, A.DownS, A.DownS,
                                        A.LeftS, A.UpS, A.UpS, A.RightS, A.RightS,
                                        A.DownS, A.LeftS, A.DownS, A.LeftS,A.UpS,
                                        A.RightS,A.DownS,A.UpS,A.UpS,A.RightS,A.DownS,
                                        A.LeftS,A.UpS,A.LeftS,A.DownS,A.RightS,
                                        A.LeftS, A.DownS, A.RightS, A.UpS, A.RightS,
                                        A.DownS, A.LeftS, A.UpS, A.UpS, A.RightS]
        where
        ff (zp,bd) sh = A.applyShift_ zp bd sh

    testBoard2 :: (A.Pos, A.Board)--The optimal solution for this board has 19 shifts
    testBoard2 = (1,A.Board bd)
        where
        bd = [(0,2),(1,0),(2,4),(3,5),(4,3),(5,8),(6,6),(7,1),(8,7)]
