module Main where

import qualified Graphics.Gloss as G
import Graphics.Gloss.Interface.Pure.Game
import qualified HomMad.Goban as Hom
import HomMad.Goban (Point (..), Color (..))
import HomMad.AI (playout)

gridWidth :: Float
gridWidth = 30

fromHomCoord :: Hom.Coord -> G.Point
fromHomCoord (row, col) =
    (fromIntegral (col - div Hom.boardSize 2) * gridWidth,
     fromIntegral (row - div Hom.boardSize 2) * (-gridWidth))

toHomCoord :: G.Point -> Hom.Coord
toHomCoord (x, y) =
    let row = round (y / (-gridWidth)) + (Hom.boardSize `div` 2)
        col = round (x / gridWidth) + (Hom.boardSize `div` 2)
    in (row, col)

grid :: G.Picture
grid = G.pictures $
       [G.line [fromHomCoord (row, 0),
                fromHomCoord (row, Hom.boardSize-1)]
        | row <- [0..Hom.boardSize-1]] ++
       [G.line [fromHomCoord (0, col),
                fromHomCoord (Hom.boardSize-1, col)]
        | col <- [0..Hom.boardSize-1]]

board :: G.Picture
board = G.rectangleSolid halfOfSize halfOfSize
    where halfOfSize = gridWidth * fromIntegral Hom.boardSize

showBoard :: Hom.Board Hom.Color -> G.Picture
showBoard b = G.pictures
              [showStone (Hom.boardRef b (row, col)) (row, col) |
               row <- [0..Hom.boardSize-1], col <- [0..Hom.boardSize-1]]

showCircle :: (Float -> G.Picture) ->
              Hom.Point Hom.Color -> Hom.Coord -> G.Picture
showCircle _ Empty _ = G.blank
showCircle cir (Point c) point = G.color (fromHomColor c)
                                 $ uncurry G.translate (fromHomCoord point)
                                 $ cir (gridWidth * 0.4)
    where fromHomColor Black = G.black
          fromHomColor White = G.white

showStone :: Hom.Point Hom.Color -> Hom.Coord -> G.Picture
showStone = showCircle G.circleSolid

showCursor :: Hom.GameStatus -> Hom.Coord -> G.Picture
showCursor st@Hom.GameStatus{Hom._turn=col} pt
    | Hom.canPut st pt = showCircle (flip G.thickCircle 3) (Point col) pt
    | otherwise        = blank

backGroundColor :: G.Color
backGroundColor = G.white

boardColor :: G.Color
boardColor = G.greyN 0.7

gridColor :: G.Color
gridColor = backGroundColor

type World = (Hom.GameStatus, Hom.Coord)

displayBoard :: World -> G.Picture
displayBoard (st@Hom.GameStatus{Hom._board=b}, cursor) =
    G.pictures [G.color boardColor $ board
               ,G.color gridColor grid
               ,showBoard b
               ,showCursor st cursor
               ]

eventHandler :: Event -> World -> World
eventHandler (EventMotion coord) (st, _) = (st, toHomCoord coord)
eventHandler (EventKey (MouseButton LeftButton) Up _ coord) (st, _) =
    let pt = toHomCoord coord
    in if Hom.canPut st pt then (Hom.putStone st pt, pt) else (st, pt)
eventHandler (EventKey (MouseButton RightButton) Up _ _) (st, pt) =
    (Hom.pass st, pt)
eventHandler (EventKey (SpecialKey KeyEnter) Up _ _) (st, pt) =
    (playout (fst pt) st, pt)
eventHandler _ w = w

main :: IO ()
main = G.play
       (G.InWindow "HomMad GUI" (700, 700) (50, 50))
       backGroundColor
       10
       (Hom.initGame, (0, 0))
       displayBoard
       eventHandler
       (\_ -> id)
