module Main where
  
import RenderGame
import PlayGame
import Control.Concurrent (newEmptyMVar)
import Graphics.Gloss.Interface.IO.Game (playIO)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  move <- newEmptyMVar
  case args of
    [] -> playIO window background 10 (initialGame, PlayerX) gameScreen (playingGame move) (step move)
    _ ->
      if head args == "X"
        then playIO window background 10 (initialGame, PlayerX) gameScreen (playingGame move) (step move)
        else playIO window background 10 (initialGame, PlayerO) gameScreen (playingGame move) (step move)
