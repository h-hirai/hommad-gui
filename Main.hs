module Main where

import qualified Graphics.Gloss as G
import Graphics.Gloss.Interface.Pure.Game
import qualified HomMad.Goban as Hom
import HomMad.Goban (Color (..))

gridWidth :: Float
gridWidth = 30

fromHomPoint :: Hom.Point -> G.Point
fromHomPoint (row, col) =
    (fromIntegral (col - div Hom.boardSize 2) * gridWidth,
     fromIntegral (row - div Hom.boardSize 2) * (-gridWidth))

toHomPoint :: G.Point -> Hom.Point
toHomPoint (x, y) =
    let row = round (y / (-gridWidth)) + (Hom.boardSize `div` 2)
        col = round (x / gridWidth) + (Hom.boardSize `div` 2)
    in (row, col)

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

showCircle :: (Float -> G.Picture) -> Hom.Color -> Hom.Point -> G.Picture
showCircle _      E     _     = G.blank
showCircle cir col point = G.color (fromHomColor col)
                           $ uncurry G.translate (fromHomPoint point)
                           $ cir (gridWidth * 0.4)
    where fromHomColor B = G.black
          fromHomColor W = G.white

showStone :: Hom.Color -> Hom.Point -> G.Picture
showStone = showCircle G.circleSolid

showCursor :: Hom.GameStatus -> Hom.Point -> G.Picture
showCursor st@Hom.GameStatus{Hom._turn=col} pt
    | Hom.canPut st pt = showCircle (flip G.thickCircle 3) col pt
    | otherwise        = blank

backGroundColor :: G.Color
backGroundColor = G.white

boardColor :: G.Color
boardColor = G.greyN 0.7

gridColor :: G.Color
gridColor = backGroundColor

type World = (Hom.GameStatus, Hom.Point)

displayBoard :: World -> G.Picture
displayBoard (st@Hom.GameStatus{Hom._board=b}, cursor) =
    G.pictures [G.color boardColor $ board
               ,G.color gridColor grid
               ,showBoard b
               ,showCursor st cursor
               ]

eventHandler :: Event -> World -> World
eventHandler (EventMotion coord) (st, _) = (st, toHomPoint coord)
eventHandler (EventKey (MouseButton LeftButton) Up _ coord) (st, _) =
    let pt = toHomPoint coord
    in if Hom.canPut st pt then (Hom.putStone st pt, pt) else (st, pt)
eventHandler _ w = w

main :: IO ()
main = G.play
       (G.InWindow "HomMad GUI" (500, 500) (50, 50))
       backGroundColor
       10
       (Hom.GameStatus Hom.emptyBoard B 0 0 Nothing, (0, 0))
       displayBoard
       eventHandler
       (\_ -> id)
