{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Lib
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD.Size


imageSize :: SizeSpec V2 Double
imageSize = dims2D 800 800


main :: IO ()
main = renderSVG "hello2.svg" imageSize (genChessboard 37 45)
