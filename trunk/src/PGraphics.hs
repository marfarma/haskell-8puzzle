--------------------------------------------------------------------------
-- Module:      PGraphics
-- Maintainer:  Carlos Colmenares
--
-- | Calculations and opertations needed to draw the 8puzzle board
-- using 'HGL'
--------------------------------------------------------------------------
module PGraphics where
    import qualified Graphics.HGL as HGL
    import qualified PAlgorithm as A

    type Point = HGL.Point

    -- | Draws a 'Board' in a 'Window', for the numbers it
    -- uses the given 'Font'
    drawBoard :: HGL.Window  -- ^ The 'Window' where to draw
                 -> HGL.Font -- ^ The font for the numbers
                 -> A.Board  -- ^ The 'Board' to draw
                 -> IO ()
    drawBoard w font bd = HGL.setGraphic w bdGraphic 
        where
        bdGraphic = boardToGraphic bd font


    -- | Transform a given board in a 'HGL' 'Graphic' to
    -- be drawn afterwards
    boardToGraphic :: A.Board -> HGL.Font -> HGL.Graphic
    boardToGraphic bd font = foldl1 HGL.overGraphic graphicTokens
        where
        graphicTokens = [ tokenToGraphic t font | t <- A.unBoard bd ]
       
    -- | Makes a 'HGL' 'Graphic' that represents the single
    -- given token in a 'Board'
    tokenToGraphic :: A.Token -> HGL.Font -> HGL.Graphic
    tokenToGraphic tk@(cell,num) font = idNum `HGL.overGraphic` rect 
        where
        idNum = numberGraphic font (tk_x,tk_y) num
        rect = rectGraphic (tk_x,tk_y) tk_color
        tk_x = (cell `mod` 3)*100 + 50
        tk_y = (cell `div` 3)*100 + 50
        tk_color
            | num == 0 = HGL.Black
            | num `mod` 2 == 0 = HGL.Blue
            | otherwise = HGL.White
                
    -- | Returns a 'HGL' 'Graphic' of a rectangle
    rectGraphic ::  Point -- ^ The point where to start the rectangle
                    -> HGL.Color -- ^ The color of the rectangle
                    -> HGL.Graphic
    rectGraphic (x,y) col = colorGraph
        where
        colorGraph = HGL.withColor col myGr
        myGr = HGL.regionToGraphic myRec
        myRec = HGL.rectangleRegion (x',y') (x'+80,y'+80)
        x' = x + 10
        y' = y + 10

    -- | Returns a 'HGL' 'Graphic' of the given number drawed
    -- with the given 'Font'
    numberGraphic :: HGL.Font -- ^ The 'Font' to use
                     -> Point -- ^ The point where to draw the number
                     -> Int -- ^ The number to draw
                     -> HGL.Graphic
    numberGraphic font (x,y) n = HGL.withFont font blackText
        where
        blackText = HGL.withTextColor rgb_black pureText
        pureText = HGL.text (x',y') $ show n
        x' = x + 35
        y' = y + 20

    -- | The black color in 'HGL' 'RGB' representation
    rgb_black :: HGL.RGB
    rgb_black = HGL.RGB 0 0 0

