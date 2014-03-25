module Main where

import qualified Graphics.Gloss as G
import qualified HomMad as Hom
import HomMad (Color (..))

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

board :: G.Picture
board = G.rectangleSolid halfOfSize halfOfSize
    where halfOfSize = gridWidth * fromIntegral Hom.boardSize

showBoard :: Hom.Board -> G.Picture
showBoard b = G.pictures
              [showStone (Hom.boardRef b (row, col)) (row, col) |
               row <- [0..Hom.boardSize-1], col <- [0..Hom.boardSize-1]]

showStone :: Hom.Color -> Hom.Point -> G.Picture
showStone E _ = G.blank
showStone c p = G.color (fromHomColor c)
                $ uncurry G.translate (fromHomPoint p)
                $ G.circleSolid (gridWidth * 0.4)
    where fromHomColor B = G.black
          fromHomColor W = G.white

backGroundColor :: G.Color
backGroundColor = G.white

boardColor :: G.Color
boardColor = G.greyN 0.7

gridColor :: G.Color
gridColor = backGroundColor

testBoard :: Hom.Board
testBoard = [[E, B, E, E, E, E, E, E, E]
            ,[B, E, E, E, E, B, E, E, E]
            ,[E, E, E, E, E, E, E, E, E]
            ,[E, E, E, E, E, B, E, E, E]
            ,[E, E, E, E, E, E, E, E, E]
            ,[E, E, E, W, E, W, E, E, E]
            ,[E, E, E, E, E, W, E, E, E]
            ,[E, E, E, E, E, E, E, E, E]
            ,[E, E, E, E, E, E, E, E, E]
            ]

main :: IO ()
main = G.display
       (G.InWindow "HomMad GUI" (500, 500) (50, 50))
       backGroundColor
       $ G.pictures [G.color boardColor $ board
                    ,G.color gridColor grid
                    ,showBoard testBoard
                    ]
