{-# LANGUAGE TupleSections #-}
module PlayGame where


import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (MouseButton), KeyState (Up), MouseButton (LeftButton), playIO)
import Control.Lens (ix, (?~))
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, threadDelay, tryTakeMVar)
import System.Random (randomRIO)
import Data.Foldable (asum)
import Data.Functor (void, (<&>))
import qualified Data.List as L

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

initialGame :: Game
initialGame = Game  {   gBoard = replicate 3 (replicate 3 Nothing),
                        state = Playing
                    }

step :: MVar Game -> Float -> (Game, Player) -> IO (Game, Player)
step move _ (b, p) =
  tryTakeMVar move <&> maybe (b, p) (,switchPlayer p)