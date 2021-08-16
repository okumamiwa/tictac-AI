module RenderGame where

import Definitions
import PlayGame ( Game(state, gBoard), State(GameOver, Playing), Board, Player(..) )
import Graphics.Gloss ( color, line, pictures, translate, Display(InWindow), Color, Picture )


--Define formato do tabuleiro
bGrid :: Picture
bGrid = pictures $ mconcat [[line  [ (x * qWidth, 0.0)
                                            , (x * qWidth, fromIntegral sHeight)
                                            ]
                                    ,line   [ (0.0, x * qHeight)
                                            , (fromIntegral sWidth, x * qHeight)
                                            ]]      
                                    | x <- [0.0 .. fromIntegral n]]

--Posiciona tabuleiro na janela do jogo
grid :: Picture
grid =  translate   (fromIntegral sWidth  / (-2))
                    (fromIntegral sHeight / (-2))
                    $ color gridColor bGrid

--Posiciona jogadores de acordo com suas cores
posPlayerColor :: Color -> Color -> Board -> Picture
posPlayerColor c1 c2 b = mconcat [translate (fromIntegral $ (x - 1) * round qWidth)
                                            (fromIntegral $ (y - 1) * round qHeight)
                                 $ case play of 
                                    PlayerX -> color c1 xCell 
                                    PlayerO -> color c2 oCell 
                                 | x <- [0..2], y <- [0..2], Just play <- [b!!x!!y]]

--Conversão do tabuleiro para Picture durante o jogo
gamePicture :: Board -> IO Picture
gamePicture b = return $ grid <> posPlayerColor pXColor pOColor b

--Conversão do tabuleiro para Picture após game over
gameOverPicture :: Maybe Player -> Board -> IO Picture
gameOverPicture w b = return $ posPlayerColor (wColor w) (wColor w) b

--Define a conversão do estado do jogo para imagem (Picture)
gameScreen :: (Game, Player) -> IO Picture
gameScreen (game, player) =
  case state game of
    Playing -> gamePicture (gBoard game)
    GameOver winner -> gameOverPicture winner (gBoard game)

--Define a janela do jogo, com nome e tamanho
window :: Display
window = InWindow "Jogo Da Velha" (sWidth, sHeight) (100, 100)

