module Main where
    import qualified Graphics.HGL as HGL
    import qualified PGraphics as G
    import qualified PAlgorithm as A
    import Control.Monad( foldM, mapM ) 
    import System.Random( randomRIO ) 

    main = do
        HGL.runGraphics $
            do
            w <- HGL.openWindowEx "8puzzle" Nothing
                 (400,400) HGL.DoubleBuffered (Just 400)

            font <- HGL.createFont (50,50)  (pi/4)  False False "helvetica"
    
            drawAndLoop w font (0, A.initBoard)

            HGL.closeWindow w

    -- | Draws the given board, takes a key from the std input and executes
    -- the requested action, then it "loops back" calling it self recursively
    drawAndLoop ::  HGL.Window  -- ^ The window where to draw stuff
                    -> HGL.Font -- ^ The font needed to draw in the window
                    -> ( A.Pos , A.Board)
                                -- ^ A tuple with the actual board and the
                                -- position of the empty cell in the board
                    -> IO ()    -- ^ The actions
    drawAndLoop w font (zPos, myBoard) = do
        G.drawBoard w font myBoard
        c <- HGL.wGetChar w
        case c of
            'w' ->  drawAndLoop w font $ recursive A.UpS
            'a' ->  drawAndLoop w font $ recursive A.LeftS
            's' ->  drawAndLoop w font $ recursive A.DownS
            'd' ->  drawAndLoop w font $ recursive A.RightS
            'e' ->  solveAndLoop w font (zPos, myBoard)
            'r' ->  randomizeAndLoop w font (zPos, myBoard)
            'q' ->  return ()
            _ ->    drawAndLoop w font (zPos, myBoard)
            where
            recursive = A.applyUserShift zPos myBoard


    -- | Find the optimal solution for the given board, apply the solution,
    -- and then go back to the main loop that receives user instructions
    solveAndLoop ::  HGL.Window  -- ^ The window where to draw stuff
                    -> HGL.Font -- ^ The font needed to draw in the window
                    -> ( A.Pos , A.Board)
                                -- ^ A tuple with the actual board and the
                                -- position of the empty cell in the board
                    -> IO ()    -- ^ The actions
    solveAndLoop w font (zPos, myBoard) = do
        --Solve the 8puzzle
        let sol = A.solve myBoard zPos
        --Report the number of steps and optimal solution
        putStrLn $ "Solved in " ++ (show (length sol)) ++ " shifts:"
        putStrLn $ show $ sol
        --Apply all the results to the actual board and loop back
        finalSol <- foldM (applyAndDraw w font) (zPos, myBoard) sol
        -- Loop back
        drawAndLoop w font finalSol

    -- | Apply all the given 'Shift' to the board, making a small pause
    -- before the move, and draw the board
    applyAndDraw ::  HGL.Window  -- ^ The window where to draw stuff
                    -> HGL.Font -- ^ The font needed to draw in the window
                    -> ( A.Pos , A.Board)
                                -- ^ A tuple with the actual board and the
                                -- position of the empty cell in the board
                    -> A.Shift  -- ^ The 'Shift' to apply
                    -> IO (A.Pos, A.Board ) -- ^ The new 'Board' and position
                                            -- of the empty cell in it
    applyAndDraw w font (zp, bd) sl = do
        HGL.getWindowTick w
        let (newZp, newBd) = A.applyShift_ zp bd sl
        G.drawBoard w font newBd
        return (newZp, newBd)


    -- | Apply 100 random moves to the board and loop back to the drawAndLoop
    -- monad
    randomizeAndLoop ::  HGL.Window  -- ^ The window where to draw stuff
                    -> HGL.Font -- ^ The font needed to draw in the window
                    -> ( A.Pos , A.Board)
                                -- ^ A tuple with the actual board and the
                                -- position of the empty cell in the board
                    -> IO ()    -- ^ The actions
    randomizeAndLoop w font (zPos, myBoard) = do
        -- Get a list of 100 random numbers in the range [0-3]
        let rangeList = take 100 $ repeat (0::Int,3::Int)
        randList <- mapM randomRIO rangeList
        -- Apply the random stuff and loop
        let newState = A.applyRandomShifts zPos myBoard randList
        drawAndLoop w font newState


