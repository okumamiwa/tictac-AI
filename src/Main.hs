{-# LANGUAGE TupleSections #-}
module Main where

import Graphics.Gloss ( display, makeColor, line, rotate, Color, Display(InWindow), Picture (Pictures), rectangleSolid, thickCircle )
import Graphics.Gloss.Data.Color ( greyN )
import Graphics.Gloss.Data.Picture (pictures, color, translate, Picture(Blank))
import Graphics.Gloss.Interface.Pure.Game
import Data.Foldable (asum)
import Data.Functor ( void, (<&>) )
import Control.Lens (ix, (?~))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar, putMVar, newEmptyMVar)
import System.Random
import Graphics.Gloss.Interface.IO.Game
import Data.Array ((!), elems)
import Data.List ( transpose )

data Player = PlayerX
            | PlayerO
            deriving (Eq, Show)

type Board = [[Maybe Player]]

data State = Playing
           | GameOver (Maybe Player)
           deriving (Eq)

data Game = Game  { gBoard :: Board,
                    state  :: State
                  }

pXColor = makeColor 255 65 0 0.6
pOColor = makeColor 85 26 175 0.6
lineColor = greyN 0.5
gridColor = makeColor 255 255 255 1

outColor :: Maybe Player -> Color
outColor (Just PlayerX) = pXColor
outColor (Just PlayerO) = pOColor
outColor Nothing = lineColor

xCell :: Picture
xCell = pictures [ rotate 45.0 $ rectangleSolid side 10.0
                 , rotate (-45.0) $ rectangleSolid side 10.0
                 ]
          where side = min cWidth cHeight * 0.75


oCell :: Picture
oCell = thickCircle (min cWidth cHeight * 0.25) 10.0


mouseCoord :: (Integral a, Integral b) => (Float, Float) -> (a, b)
mouseCoord (x, y) = ( floor ((y + (fromIntegral sHeight * 0.5)) / cHeight)
                    , floor ((x + (fromIntegral sHeight * 0.5)) / cWidth)
                    )

{-switch :: Game -> Game
switch game =
  case player game of
    PlayerX -> game { player = PlayerO }
    PlayerO -> game { player = PlayerX }-}

playAI :: MVar Game -> Game -> IO ()
playAI move g = void $ forkIO $ do
  randomRIO (100000, 100000) >>= threadDelay
  let b = gBoard g
      s = state g
      plays = [ ((ix x . ix y) ?~ PlayerO) b
              | x <- [0..2]
              , y <- [0..2]
              , Nothing <- [ (b !! x) !! y ]
              ]
  case plays of
    [] -> do
      putMVar move g
    _  -> do
      newB <- (plays !!) <$> randomRIO (0, length plays - 1)
      putMVar move Game{gBoard=newB, state=s}


changeGame :: MVar Game -> Event-> (Game, Player)-> IO (Game, Player)
changeGame move (EventKey (MouseButton LeftButton) Up _ (x, y)) (g, PlayerX) = do
  let snap = (+1) . min 1 . max (-1) . fromIntegral . floor . (/100) . (+50)
      gx   = snap x
      gy   = snap y
      b    = gBoard g
      s    = state g
    in case (b !! gx) !! gy of
        Just _  -> return (g, PlayerX)
        Nothing -> do
          let newB = ((ix gx . ix gy) ?~ PlayerX) b
              newGame = Game{gBoard=newB, state = s}
          playAI move newGame
          return (newGame, PlayerO)
changeGame _ _ (g, p) = return (g, p)

snapPictureToCell pic (x, y) = translate i j pic
  where i = fromIntegral y * cWidth + cWidth *  0.5
        j = fromIntegral x * cHeight + cHeight * 0.5

boardGrid :: Picture
boardGrid =
  pictures $ concatMap (\x -> [line [ (x * cWidth, 0.0)
                                    , (x * cWidth, fromIntegral sHeight)
                                    ]
                              ,line [ (0.0, x * cHeight)
                                    , (fromIntegral sWidth, x * cHeight)
                                    ]
                              ])
              [0.0 .. fromIntegral n]

n :: Int
n = 3

sWidth :: Int
sWidth = 640

sHeight :: Int
sHeight = 480

cWidth :: Float
cWidth = fromIntegral sWidth / fromIntegral n

cHeight :: Float
cHeight = fromIntegral sHeight / fromIntegral n

indexRange :: ((Int, Int), (Int, Int))
indexRange = ((0, 0), (n-1, n-1))

full :: [Maybe Player] -> Maybe Player
full (p:ps) | all (== p) ps = p
full _                      = Nothing

winner :: Board -> Maybe Player
winner b = asum $ map full $ rows ++ cols ++ diags
    where rows  = b
          cols  = transpose b
          diags = [[head(head b), b!!1!!1,b!!2!!2],[head b!!2, b!!1!!1,head (b!!2)]]


countCells :: Board -> Int
countCells = length . filter (Nothing ==) . concat

checkOver :: Game -> Game
checkOver game
  | Just p <- winner b = game {state = GameOver $ Just PlayerX}
  | countCells b == 0 = game {state = GameOver Nothing}
  | otherwise = game
    where b = gBoard game

grid :: Picture
grid = translate (fromIntegral sWidth  * (-0.5))
                 (fromIntegral sHeight * (-0.5))
                 $ color gridColor boardGrid

plays :: Board -> Picture
plays b = mconcat
          [ translate (fromIntegral $ (x - 1) * round cWidth)
                      (fromIntegral $ (y - 1) * round cHeight) $
              case play of
                PlayerX -> color pXColor xCell
                PlayerO -> color pOColor oCell
          | x <- [0..2]
          , y <- [0..2]
          , Just play <- [ (b !! x) !! y ]
          ]


gamePicture :: (Board, Player) -> IO Picture
gamePicture (b, p) = return $ grid <> plays b

gameOverPicture :: Maybe Player -> Board -> IO Picture
gameOverPicture winner board = return $ color (outColor winner) (plays board)

gameScreen :: (Game, Player) -> IO Picture
gameScreen (game, player) =
  case state game of 
    Playing         -> gamePicture (gBoard game, player)
    GameOver winner -> gameOverPicture winner(gBoard game)

initialGame :: Game
initialGame = Game { gBoard = replicate 3(replicate 3 Nothing )
                   , state = Playing
                   }

background :: Color
background = makeColor 0 0 0 1

window :: Display
window = InWindow "Jogo Da Velha" (sWidth, sHeight) (100, 100)

step :: MVar Game -> Float -> (Game, Player) -> IO (Game, Player)
step move _ (b, PlayerO) =
  tryTakeMVar move <&> maybe (b, PlayerO) (, PlayerX)
step _ _ state = return state

main :: IO ()
main = do
  move <- newEmptyMVar
  playIO window background 10 (initialGame, PlayerX) gameScreen (changeGame move) (step move)
