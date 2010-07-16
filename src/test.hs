--------------------------------------------------------------------------
-- file:        test.hs
-- Maintainer:  Carlos Colmenares
--
-- | QuickCheck unary tests for checking the procedures defined in
-- PAlgorithm
--------------------------------------------------------------------------

import Test.QuickCheck
import Data.List( permutations )
import PAlgorithm
import Control.Monad

main = do
    quickCheck ( prop_doubleSwap   :: Board -> Position -> Position -> Bool ) 
    quickCheck ( prop_mirrorShifts :: Shift -> Position -> Property )
    quickCheck ( prop_mirrorShiftsBoard :: Board -> Shift -> Position -> Property )
    quickCheck ( prop_mirrorShiftsBoard2 :: Board -> Shift -> Position -> Property )
    quickCheck ( prop_possibleShiftCheck :: Position -> Bool )
    quickCheck ( prop_posAndCoord :: Position -> Bool )
    quickCheck ( prop_coordAndPos :: Coordinate -> Bool )
    quickCheck ( prop_solveTest :: Board -> Property )
    -- Check the boards
    putStrLn "\nChecking the test boards for the aStar\n\nTestBoard 1:"
    putStrLn $ show $ snd testBoard
    let sol = solve (snd testBoard) (fst testBoard)
    putStrLn $ "Solved in " ++ (show (length sol)) ++ " shifts:"
    putStrLn $ show sol

    putStrLn "\nTestBoard 2\n"
    putStrLn $ show $ snd testBoard2
    let sol2 = solve (snd testBoard2) (fst testBoard2)
    putStrLn $ "Solved in " ++ (show (length sol2)) ++ " shifts:"
    putStrLn $ show sol2
    
    

-- | Make 'Board' an instance of Arbitrary,
-- For creating an arbitrary Board, it's necesary
-- to make a random permutation of the list [0..8]
instance Arbitrary Board where
    arbitrary = do
        randPer <- elements $ permutations [0..8]
        let randBoard = [(i, randPer!!i) | i <- [0..8]]
        liftM Board (return randBoard)

instance Arbitrary Shift where
    arbitrary = elements [LeftS, RightS, UpS, DownS]

-- | We make 'Pos' a newtype, so there won't be problems
-- making arbitrary numbers
newtype Position = Position Pos
instance Show Position where
    show (Position p) = show p

instance Arbitrary Position where
    arbitrary = do
        x <- elements ([0..8]::[Pos])
        liftM Position (return x)

-- | We make 'Coord' a newtype, so there won't be problems
-- making arbitrary numbers
newtype Coordinate = Coordinate Coord
instance Show Coordinate where
    show (Coordinate p) = show p

instance Arbitrary Coordinate where
    arbitrary = do
        x <- elements ([0..2]::[Int])
        y <- elements ([0..2]::[Int])
        liftM Coordinate (return (x,y))

-----------------------------------------------------------

-- | Making a swap twice is the same as never do it
prop_doubleSwap bd (Position x) (Position y) =
   boardSwap (boardSwap bd x y) x y == bd 

-- | Making a Shift and it's mirror Shift in any position
-- is the same as doing nothing
prop_mirrorShifts sh (Position ps) = 
    isPossibleShift ps sh ==>
    ps == shiftPos (shiftPos ps sh) (mirror sh)

-- | Making mirror Shifts in a board (at any position),
-- if both are possible, is the same as doing nothing
prop_mirrorShiftsBoard bd sh (Position ps) =
    isPossibleShift ps sh ==>
    bd == applyShift mirrorPos (applyShift ps bd sh) mirrorShift
        where
        mirrorPos = shiftPos ps sh
        mirrorShift = mirror sh

-- | Making a non possible shift and then it's mirror one,
-- is the same as doing only the second one
prop_mirrorShiftsBoard2 bd sh (Position ps) =
    not (isPossibleShift ps sh) ==>
    applyShift ps bd mirrorShift == 
    applyShift ps (applyShift ps bd sh) mirrorShift
        where
        mirrorShift = mirror sh

-- | Check that all the shifts declared as be possible
-- by the function 'possibleShifts' are really possible
prop_possibleShiftCheck (Position ps) =
    foldl (&&) True l
        where
        l = map (isPossibleShift ps) (possibleShifts ps)

-- | Check that the transformation posToCord and cordToPos
-- are inverses
prop_posAndCoord (Position ps) =
    ps == coordToPos (posToCoord ps)

-- | Check that the transformation posToCord and cordToPos
-- are inverses
prop_coordAndPos (Coordinate co) =
    co == posToCoord (coordToPos co)

-- | Check that after solving, the result is the initboard
prop_solveTest bd =
    isSolvable bd ==>
    initBoard == solvedBoard
        where
        zp = findZeroPos bd
        shiftList = solve bd zp
        (_,solvedBoard) = foldl ff (zp,bd) shiftList
            where
            ff (z,b) = applyShift_ z b

-----------------------------------------------------------

--Test boards for testing the aStar
--
testBoard :: (Pos, Board) --The optimal solution for this board has 28 shifts
testBoard = foldl ff (0,initBoard) [UpS, UpS, LeftS, DownS, DownS,
                                    LeftS, UpS, UpS, RightS, RightS,
                                    DownS, LeftS, DownS, LeftS,UpS,
                                    RightS,DownS,UpS,UpS,RightS,DownS,
                                    LeftS,UpS,LeftS,DownS,RightS,
                                    LeftS, DownS, RightS, UpS, RightS,
                                    DownS, LeftS, UpS, UpS, RightS]
    where
    ff (zp,bd) sh = applyShift_ zp bd sh

testBoard2 :: (Pos, Board)--The optimal solution for this board has 19 shifts
testBoard2 = (1,Board bd)
    where
    bd = [(0,2),(1,0),(2,4),(3,5),(4,3),(5,8),(6,6),(7,1),(8,7)]
