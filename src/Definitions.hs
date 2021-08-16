module Definitions where

import Graphics.Gloss (Color, Display (InWindow), line, makeColor, rectangleSolid, rotate, thickCircle)
import Graphics.Gloss.Data.Picture (Picture, color, pictures, translate)
import PlayGame ( Player(..) )

n :: Int
n = 3

--Largura da Tela
sWidth :: Int
sWidth = 640

--Altura da Tela
sHeight :: Int
sHeight = 480

--Largura quadrado menor
qWidth :: Float
qWidth = fromIntegral sWidth / fromIntegral n

--Algura quadrado menor
qHeight :: Float
qHeight = fromIntegral sHeight / fromIntegral n


indexRange :: ((Int, Int), (Int, Int))
indexRange = ((0, 0), (n -1, n -1))

--Define cor do X (vermelho)
pXColor :: Color
pXColor = makeColor 255 0 0 0.6

--Define cor do O (azul)
pOColor :: Color
pOColor = makeColor 0 0 255 0.6

--Define cor da grade do tabuleiro
gridColor :: Color
gridColor = makeColor 255 255 255 1

--Define cor através do ganhador (roxo para empate)
wColor :: Maybe Player -> Color
wColor (Just PlayerX) = pXColor
wColor (Just PlayerO) = pOColor
wColor Nothing = makeColor 255 0 255 0.6

--Define formato do X
xCell :: Picture
xCell = pictures [ rotate (-45.0) $ rectangleSolid side 10.0
                 , rotate   45.0  $ rectangleSolid side 10.0
                 ]
          where side = qHeight * 0.75

--Define formato do O
oCell :: Picture
oCell = thickCircle radius 10.0
            where radius = qHeight * 0.25