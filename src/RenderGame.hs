module RenderGame where

import PlayGame
import Definitions
import Graphics.Gloss



--Define formato do tabuleiro
boardGrid :: Picture
boardGrid =
  pictures $ concatMap (\x -> [line [ (x * qWidth, 0.0)
                                    , (x * qWidth, fromIntegral sHeight)
                                    ]
                              ,line [ (0.0, x * qHeight)
                                    , (fromIntegral sWidth, x * qHeight)
                                    ]
                              ])
              [0.0 .. fromIntegral n]


plays :: Board -> Picture
plays b = mconcat
          [ translate (fromIntegral $ (x - 1) * round qWidth)
                      (fromIntegral $ (y - 1) * round qHeight) $
              case play of
                PlayerX -> color pXColor xCell
                PlayerO -> color pOColor oCell
          | x <- [0..2]
          , y <- [0..2]
          , Just play <- [ b !! x !! y ]
          ]

playOver :: Maybe Player -> Board -> Picture
playOver w b = mconcat  [translate  (fromIntegral $ (x - 1) * round qWidth)
                                    (fromIntegral $ (y - 1) * round qHeight)
                        $ case play of
                            PlayerX -> color (wColor w) xCell
                            PlayerO -> color (wColor w) oCell
                        | x <- [0..2]
                        , y <- [0..2]
                        , Just play <- [b!!x!!y]]

grid :: Picture
grid =
  translate
    (fromIntegral sWidth * (-0.5))
    (fromIntegral sHeight * (-0.5))
    $ color gridColor boardGrid

gameScreen :: (Game, Player) -> IO Picture
gameScreen (game, player) =
  case state game of
    Playing -> gamePicture (gBoard game, player)
    GameOver winner -> gameOverPicture winner (gBoard game)



gamePicture :: (Board, Player) -> IO Picture
gamePicture (b, p) = return $ grid <> plays b

gameOverPicture :: Maybe Player -> Board -> IO Picture
gameOverPicture winner board = return $ playOver winner board

background :: Color
background = makeColor 0 0 0 1

window :: Display
window = InWindow "Jogo Da Velha" (sWidth, sHeight) (100, 100)

