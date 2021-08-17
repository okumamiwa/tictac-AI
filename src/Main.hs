module Main where
  
import RenderGame ( gameScreen, window )
import PlayGame( Game, Player(PlayerO, PlayerX), playingGame, step, initialGame )
import Definitions ( background )
import Control.Concurrent (MVar, newEmptyMVar)
import Graphics.Gloss.Interface.IO.Game (playIO)
import System.Environment (getArgs)

playWithX :: MVar Game -> IO ()
playWithX move = playIO window background 10 (initialGame, PlayerX) gameScreen (playingGame move) (step move)

playWithO :: MVar Game -> IO ()
playWithO move = playIO window background 10 (initialGame, PlayerO) gameScreen (playingGame move) (step move)

main :: IO ()
main = do
  args <- getArgs
  move <- newEmptyMVar
  case args of
    [] -> playWithX move
    _  -> if head args == "X" 
          then playWithX move
          else playWithO move
