--------------------------------------------------------------------------
-- Module:      PAlgorithm
-- Maintainer:  Carlos Colmenares
--
-- | Back-End operations for the 8 puzzle: Data structures, types, and
-- functions for making operations on a board and calculating the shortest
-- path from any state up to the goal state.
--------------------------------------------------------------------------
module PAlgorithm where
    import qualified Data.PurePriorityQueue as PQ
    import qualified Data.Array.Unboxed as UA
    import qualified Data.Set as S
    import Data.List(intercalate, foldl')

    -- | A position in the board, it will always be an
    -- 'Int' in the range [0, 8] 
    --
    -- >  0 | 1 | 2
    -- > -----------
    -- >  3 | 4 | 5
    -- > -----------
    -- >  6 | 7 | 8
    type Pos = Int

    -- | A coordinate of a position in the board, each
    -- of the numbers will always be an 'Int' in the range
    -- [0, 3]
    --
    -- >  (0,0) | (0,1) | (0,2)
    -- > -----------------------
    -- >  (1,0) | (1,1) | (1,2)
    -- > -----------------------
    -- >  (2,0) | (2,1) | (2,2)
    type Coord = (Int,Int)

    -- | A token is a pair (Position, Number of token),
    -- a token /(2,3)/ represents that the token number 3
    -- is in the second position of the board.
    type Token = (Pos,Int)

    -- | The representation of a board. It's a collection
    -- of tokens (one for every position). This type will
    -- be an instance of 'Eq', 'Ord' and 'Show'.
    newtype Board = 
        Board {
            unBoard::[Token]    -- ^ List of tokens, a list is not the
                                -- best structutre for this (swaping
                                -- two elements is really uneficient)
        }
        deriving(Eq)

    -- | How to compare a board
    instance Ord Board where
        compare (Board x) (Board y) = compare x y

    -- | How to show a board
    instance Show Board where
        show = showBoard

    -- | Function for pretty printing a 'Board'. Used as
    -- the 'show' function of a 'Board'
    showBoard :: Board -> String
    showBoard (Board tkL) = intercalate "\n" bdLines
        where
        bdLinesTok = take 3 [take 3 l| l <- iterate (drop 3) tkL]
        bdLinesNum = map (map snd) bdLinesTok
        bdLines = map show bdLinesNum

    -- | The initial 'Board' or goal 'Board'.
    initBoard :: Board
    initBoard = Board [ (i,i) | i <- [0..8] ]

    -- | Definition of a node, used for the A* algorithm.
    -- For debugging purposes, PNode will be instacne of
    -- 'Show'.
    data PNode = PNode {
        steps :: Int,   -- ^ The moves made for arriving to
                        -- this node in the A*
        zeroPos :: Pos, -- ^ The position of the empty cell
                        -- in the board, used for simplifying calculations
        board :: Board, -- ^ The board of the PNode (Actual state)
        path :: [Shift] -- ^ The path followed to reach this state
    }

    -- | How to show a PNode
    instance Show PNode where
        show = showPNode

    -- | Function for pretty printing a 'PNode'
    showPNode :: PNode -> String
    showPNode (PNode s zp bd _) = (show bd) ++ "\n" ++ (show s) ++ " " ++ (show zp) ++ "\n"
    

    -- | Represents a move of a token to an empty cell
    data Shift = LeftS | RightS | UpS | DownS
        deriving (Show, Enum)

    shiftToCoord :: Shift -> Coord
    shiftToCoord s = case s of
        LeftS -> (1,0)
        RightS -> (-1,0)
        UpS -> (0,1)
        DownS -> (0,-1)

    type BPriorityQ = PQ.MinMaxQueue Int PNode
    type BSet = S.Set Board

    -- | Takes a 'Shift' and returns its counterpart
    mirror :: Shift -> Shift
    mirror sh = case sh of
        LeftS -> RightS
        RightS -> LeftS
        UpS -> DownS
        DownS -> UpS

    manhattanH :: Board -> Int
    manhattanH bd = (sum . map dist) board
        where
        board = unBoard bd
    
    dist :: Token -> Int
    dist (cell, num) = abs (xn - xc) + abs (yn - yc)
        where
        (xc, yc) = posToCoord cell
        (xn, yn) = posToCoord num

    posToCoord :: Pos -> Coord
    posToCoord n = (x,y)
        where
        x = n `mod` 3
        y = n `div` 3

    coordToPos :: Coord -> Pos
    coordToPos (x,y) = y*3 + x

    isFinal :: Board -> Bool
    isFinal bd = mhDist == 0
        where
        mhDist = manhattanH bd

    moves :: PNode -> [PNode]
    moves nod@(PNode s zp bd pt) = map toPNode newBoardsZipped
        where
        ps = possibleShifts zp
        newBoards = map (applyShift_ zp bd) ps
        newBoardsZipped = zip newBoards ps
        toPNode ((nzp,nbd),sh) =
            PNode {
                steps = s+1,
                zeroPos = nzp,
                board = nbd,
                path = sh:pt
            }

    possibleShifts :: Pos -> [Shift]
    possibleShifts x = (UA.!) possibleShPreproc x

    possibleShPreproc :: UA.Array Pos [Shift]
    possibleShPreproc = UA.array (0,8) [ (i, shiftList i) | i <- [0..8] ]

    shiftList :: Pos -> [Shift]
    shiftList n = leftRight ++ upDown
        where
        (x,y) = posToCoord n
        leftRight
            | x == 0 = [LeftS]
            | x == 2 = [RightS]
            | otherwise = [LeftS, RightS]
        upDown
            | y == 0 = [UpS]
            | y == 2 = [DownS]
            | otherwise = [UpS, DownS]

    applyShift :: Pos -> Board -> Shift -> Board
    applyShift zp bd sh
        | isPossibleShift zp sh = newBoard
        | otherwise = bd
            where
            newBoard = snd $ applyShift_ zp bd sh

    applyShift_ :: Pos -> Board -> Shift -> (Pos, Board)
    applyShift_ zp bd sh = (sp, swappedBd)
        where
        sp = shiftPos zp sh
        swappedBd = boardSwap bd zp sp

    -- | Applies the given shift to the board only if
    -- possible
    applyUserShift ::   Pos -- ^ The position of the empty cell in the board
                        -> Board -- ^ The 'Board' where to apply the 'Shift'
                        -> Shift -- ^ The 'Shift' to be applied
                        -> (Pos, Board) -- ^ The resulting 'Board' and position
                                        -- of the zero token
    applyUserShift zp bd sh =
        if isPossibleShift zp sh
            then (applyShift_ zp bd sh)
            else (zp, bd)

    isPossibleShift :: Pos -> Shift -> Bool
    isPossibleShift zp sh = inRange (xz+xsh) && inRange (yz+ysh)
        where
        (xsh, ysh) = shiftToCoord sh
        (xz, yz) = posToCoord zp
        inRange n = 0 <= n && n < 3

    -- | Takes a list of random numbers in the range [0-3],
    -- transforms it to shifts, and applies them to the board,
    -- if the shift is not valid, it's reverse movement is
    -- made instead
    applyRandomShifts :: Pos -- ^ The position of the empty cell in the board
                        -> Board -- ^ The 'Board' where to apply the 'Shift'
                        -> [Int] -- ^ The list of random numbers
                        -> (Pos, Board) -- ^ The resulting 'Board' and position
                                        -- of the zero token
    applyRandomShifts zp bd rlt = foldl' checkAndApply (zp, bd) shiftList
        where
        shiftList = map toEnum rlt
        checkAndApply (pos, board) sh =
            if isPossibleShift pos sh
                then applyShift_ pos board sh
                else applyShift_ pos board (mirror sh)

    shiftPos :: Pos -> Shift -> Pos
    shiftPos p sh = coordToPos (xp + xsh, yp + ysh)
        where
        (xp, yp) = posToCoord p
        (xsh, ysh) = shiftToCoord sh

    boardSwap :: Board -> Pos -> Pos -> Board
    boardSwap (Board bd) x y = Board nbd
        where
        nbd = bef ++ [(mini,maxiTk)] ++ med ++ [(maxi,miniTk)] ++ aft 
        (mini,maxi) = if x < y then (x,y) else (y,x)
        bef = take mini bd
        med = (drop (mini+1) . take maxi) bd
        aft = drop (maxi + 1) bd
        (_,miniTk) = bd!!mini
        (_,maxiTk) = bd!!maxi



    solve :: Board -> Pos -> [Shift]
    solve bd zp = aStar initPQ initS
        where
        initPQ = PQ.singleton initPN initMD
        initPN =
            PNode {
                steps = 0,
                zeroPos = zp,
                board = bd,
                path = []
            }
        initMD = manhattanH bd
        initS = S.empty

    aStar :: BPriorityQ -> BSet -> [Shift]
    aStar pq set  
        | isFinal (board mini) = (reverse . path) mini
        | otherwise = aStar newPQ newSet
        where
            Just ( (mini,heu) , pqSansMini ) = PQ.minView pq
            miniBd = board mini
            newPQ = foldl (checkAndInsert set miniBd) pqSansMini $ moves mini
            newSet = S.insert miniBd set

    checkAndInsert :: BSet -> Board -> BPriorityQ -> PNode -> BPriorityQ
    checkAndInsert set bd pq nod
        | S.member bd set = pq
        | otherwise = insertNodeToPQ pq nod

    insertNodeToPQ :: BPriorityQ -> PNode -> BPriorityQ
    insertNodeToPQ pq pnod = PQ.insert pnod nodHeuristic pq
        where
        nodHeuristic = steps pnod + manhattanH (board pnod)
        
    showSequence :: Pos -> Board -> [Shift] -> IO()
    showSequence zp bd ls = do
        putStrLn $ (show bd ++ "\n")
        case ls of 
            [] -> return ()
            (x:xs) -> showSequence newZp newBd xs
                where
                (newZp,newBd) = applyShift_ zp bd x


