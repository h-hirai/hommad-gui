module Main where

import qualified Graphics.Gloss as G
import qualified HomMad as Hom

backGroundColor :: G.Color
backGroundColor = G.white

gridWidth :: Float
gridWidth = 30

fromHomPoint :: Hom.Point -> G.Point
fromHomPoint (row, col) =
    (fromIntegral (col - div Hom.boardSize 2) * gridWidth,
     fromIntegral (row - div Hom.boardSize 2) * (-gridWidth))

grid :: G.Picture
grid = G.pictures $
       [G.line [fromHomPoint (row, 0),
                fromHomPoint (row, Hom.boardSize-1)]
        | row <- [0..Hom.boardSize-1]] ++
       [G.line [fromHomPoint (0, col),
                fromHomPoint (Hom.boardSize-1, col)]
        | col <- [0..Hom.boardSize-1]]

main :: IO ()
main = G.display
       (G.InWindow "HomMad GUI" (500, 500) (50, 50))
       backGroundColor grid
