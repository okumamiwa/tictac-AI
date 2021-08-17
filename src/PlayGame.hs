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

--Troca de player de acordo com atual
switchPlayer :: Player -> Player
switchPlayer p
  | p == PlayerX = PlayerO
  |otherwise = PlayerX

--Faz conversão da coordenada para quadrante do jogo
conv :: Float -> Int
conv = (+1) . min 1 . max (-1) . fromIntegral . floor . (/100) . (+50)

--Conta espaços vazios not tabuleiro
countEmpty :: Board -> Int
countEmpty = length . filter (Nothing ==) . concat

--Verifica se lista de jogadores (linha, coluna ou diagonal) contém apenas um tipo
full :: [Maybe Player] -> Maybe Player
full (p:ps) | all (== p) ps = p
full _                      = Nothing

--Função que analisa o tabuleiro e retorna o jogador vencedor
winner :: Board -> Maybe Player
winner b = asum $ map full $ rows ++ cols ++ diags
    where rows  = b
          cols  = L.transpose b
          diags = [[head(head b), b!!1!!1,b!!2!!2],[head b!!2, b!!1!!1,head (b!!2)]]

--Checa se o jogo terminou
checkOver :: Game -> Game
checkOver game
    | Just p <- winner b = game {state = GameOver $ Just p}
    | countEmpty b == 0  = game {state = GameOver Nothing}
    | otherwise = game
        where b = gBoard game

--Faz jogada do computador
playAI :: MVar Game -> Game -> Player -> IO ()
playAI move g p = void $ forkIO $ do
  threadDelay 100000
  let b = gBoard g
      s = state g
      plays = [ ((ix x . ix y) ?~ p) b | x <- [0..2], y <- [0..2]
              , Nothing <- [ b !! x !! y ]
              ]
  case plays of
    [] -> putMVar move g
    _  -> do newB <- (plays !!) <$> randomRIO (0, length plays - 1)
             putMVar move $ checkOver Game{ gBoard = newB, state = s }

--Verifica estado do jogo antes de fazer jogada do computador
aiTurn :: MVar Game -> Game -> Player -> IO (Game, Player)
aiTurn move g p = do
  case state g of
    Playing    -> do
      playAI move g p
      return (checkOver g, p)
    GameOver _ -> do
      return (g, switchPlayer p)

--Função para lidar com os eventos de input
--Se o jogador for O e o tabuleiro estiver vazio, faz a jogada do computador primeiro
playingGame :: MVar Game -> Event -> (Game, Player) -> IO (Game, Player)
playingGame move (EventKey (MouseButton LeftButton) Up _ (x,y)) (g, p) =
    case state g of
        Playing -> do
            let b = gBoard g
                s = state g
                (bx, by) = (conv x, conv y)
             in case (b !! bx) !! by of
                    Just _  -> return (g, p)
                    Nothing -> do
                    let newB = ((ix bx . ix by) ?~ p) b
                        newGame = checkOver Game{gBoard=newB, state=s}
                    aiTurn move newGame $ switchPlayer p
        GameOver _ -> return (initialGame, p)
playingGame move _ (g, p) =
    if p == PlayerO && countEmpty (gBoard g) == 9 then aiTurn move g $ switchPlayer p
    else return (g, p)

--Função para modificar o jogo a cada iteração, dependendo do tempo passado
step :: MVar Game -> Float -> (Game, Player) -> IO (Game, Player)
step move _ (b, p) =
  tryTakeMVar move <&> maybe (b, p) (,switchPlayer p)

--Define estágio inicial do jogo
initialGame :: Game
initialGame = Game  {   gBoard = replicate 3 (replicate 3 Nothing),
                        state = Playing
                    }