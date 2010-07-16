module PGraphics where
    import qualified Graphics.HGL as HGL
    import qualified PAlgorithm as A

    type Point = HGL.Point

    drawBoard :: HGL.Window -> HGL.Font -> A.Board -> IO ()
    drawBoard w font bd = HGL.setGraphic w bdGraphic 
        where
        bdGraphic = boardToGraphic bd font


    boardToGraphic :: A.Board -> HGL.Font -> HGL.Graphic
    boardToGraphic bd font = foldl1 HGL.overGraphic graphicTokens
        where
        graphicTokens = [ tokenToGraphic t font | t <- A.unBoard bd ]
       
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
                

    rectGraphic :: Point -> HGL.Color -> HGL.Graphic
    rectGraphic (x,y) col = colorGraph
        where
        colorGraph = HGL.withColor col myGr
        myGr = HGL.regionToGraphic myRec
        myRec = HGL.rectangleRegion (x',y') (x'+80,y'+80)
        x' = x + 10
        y' = y + 10

    numberGraphic :: HGL.Font -> Point -> Int -> HGL.Graphic
    numberGraphic font (x,y) n = HGL.withFont font blackText
        where
        blackText = HGL.withTextColor rgb_black pureText
        pureText = HGL.text (x',y') $ show n
        x' = x + 35
        y' = y + 20

    rgb_black :: HGL.RGB
    rgb_black = HGL.RGB 0 0 0

