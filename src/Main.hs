{-# LANGUAGE TupleSections #-}
module Main where

import Graphics.Gloss ( makeColor, line, rotate, Color, Display(InWindow), Picture (Pictures), rectangleSolid, thickCircle )
import Graphics.Gloss.Data.Picture (pictures, color, translate, Picture)
import Control.Concurrent (MVar, forkIO, threadDelay, putMVar, tryTakeMVar, newEmptyMVar)
import Graphics.Gloss.Interface.IO.Game ( Key(MouseButton), Event(EventKey), playIO, KeyState(Up), MouseButton(LeftButton) )
import Data.Functor (void, (<&>))
import System.Random ( randomRIO )
import Control.Lens (ix, (?~))
import qualified Data.List as L
import Data.Array ((!), elems)
import Data.Foldable (asum)
import System.Environment ( getArgs )


data Player = PlayerX
            | PlayerO
            deriving (Eq, Show)

type Board = [[Maybe Player]]

data State = Playing
           | GameOver (Maybe Player)
           deriving (Eq, Show)

data Game = Game  { gBoard :: Board,
                    state  :: State
                  } deriving (Show)

pXColor :: Color
pXColor = makeColor 255 0 0 0.6

pOColor :: Color
pOColor = makeColor 0 0 255 0.6

lineColor :: Color
lineColor = makeColor 255 0 255 0.6

gridColor :: Color
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

switchPlayer :: Player -> Player
switchPlayer p
  | p == PlayerX = PlayerO
  |otherwise = PlayerX

playAI :: MVar Game -> Game -> Player -> IO ()
playAI move g p = void $ forkIO $ do
  randomRIO (100000, 100000) >>= threadDelay
  let b = gBoard g
      s = state g
      plays = [ ((ix x . ix y) ?~ p) b
              | x <- [0..2]
              , y <- [0..2]
              , Nothing <- [ b !! x !! y ]
              ]
  case plays of
    [] ->
      putMVar move g
    _  -> do
      newB <- (plays !!) <$> randomRIO (0, length plays - 1)
      putMVar move $ checkOver Game{gBoard=newB, state=s}

conv :: Float -> Int
conv = (+1) . min 1 . max (-1) . fromIntegral . floor . (/100) . (+50)

aiTurn :: MVar Game -> Game -> Player -> IO (Game, Player)
aiTurn move g p = do
  case state g of
    Playing    -> do
      playAI move g p
      return (checkOver g, p)
    GameOver _ -> do
      return (g, switchPlayer p)

playingGame :: MVar Game -> Event -> (Game, Player) -> IO (Game, Player)
playingGame move (EventKey (MouseButton LeftButton) Up _ (x,y)) (g, p) =
  if p == PlayerO && countCells (gBoard g) == 9 then aiTurn move g $ switchPlayer p
  else
    case state g of
      Playing -> do
        let b = gBoard g
            s = state g
            (bx, by) = (conv x, conv y)
          in  case (b !! bx) !! by of
                Just _  -> return (g, p)
                Nothing -> do
                  let newB = ((ix bx . ix by) ?~ p) b
                      newGame = checkOver Game{gBoard=newB, state=s}
                  aiTurn move newGame $ switchPlayer p
      GameOver _ -> return (initialGame, p)
playingGame _ _ (g, p) = return (g, p)

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
          cols  = L.transpose b
          diags = [[head(head b), b!!1!!1,b!!2!!2],[head b!!2, b!!1!!1,head (b!!2)]]


countCells :: Board -> Int
countCells = length . filter (Nothing ==) . concat

checkOver :: Game -> Game
checkOver game
  | Just p <- winner b = game {state = GameOver $ Just p}
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
          , Just play <- [ b !! x !! y ]
          ]

playOver :: Maybe Player -> Board -> Picture
playOver w b = mconcat  [translate  (fromIntegral $ (x - 1) * round cWidth)
                                    (fromIntegral $ (y - 1) * round cHeight)
                        $ case play of
                            PlayerX -> color (outColor w) xCell
                            PlayerO -> color (outColor w) oCell
                        | x <- [0..2]
                        , y <- [0..2]
                        , Just play <- [b!!x!!y]]

gamePicture :: (Board, Player) -> IO Picture
gamePicture (b, p) = return $ grid <> plays b

gameOverPicture :: Maybe Player -> Board -> IO Picture
gameOverPicture winner board = return $ playOver winner board

gameScreen :: (Game, Player) -> IO Picture
gameScreen (game, player) =
  case state game of
    Playing         -> gamePicture (gBoard game, player)
    GameOver winner -> gameOverPicture winner (gBoard game)

initialGame :: Game
initialGame = Game { gBoard = replicate 3(replicate 3 Nothing )
                   , state = Playing
                   }

background :: Color
background = makeColor 0 0 0 1

window :: Display
window = InWindow "Jogo Da Velha" (sWidth, sHeight) (100, 100)

step :: MVar Game -> Float -> (Game, Player) -> IO (Game, Player)
step move _ (b, p) =
  tryTakeMVar move <&> maybe (b, p) (, switchPlayer p)

main :: IO ()
main = do
  args <- getArgs
  move <- newEmptyMVar
  case args of 
    [] -> playIO window background 10 (initialGame, PlayerX) gameScreen (playingGame move) (step move)
    _  -> if head args == "X" 
          then playIO window background 10 (initialGame, PlayerX) gameScreen (playingGame move) (step move)
          else playIO window background 10 (initialGame, PlayerO) gameScreen (playingGame move) (step move)

