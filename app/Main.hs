{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Lib

import Data.Semigroup ((<>))
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (option, value)
import Diagrams.TwoD.Size
import Options.Applicative 


data Inputs = Inputs
  { fileName    :: String
  , imageWidth  :: Int
  , imageHeight :: Int
  , chessMinC   :: Int
  , chessMaxC   :: Int }


inputs :: Parser Inputs
inputs = Inputs
      <$> strOption
          ( long "output-svg-file-name"
         <> metavar "FILE"
         <> help "Output name of generated svg file" 
         <> value "hex_chessboard.svg"
         <> showDefault)
      <*> option auto
          ( long "imageWidth"
         <> help "output image width"
         <> short 'w'
         <> showDefault
         <> value 800
         <> metavar "INT")
      <*> option auto
          ( long "imageHeight"
         <> help "output image height"
         <> short 'h'
         <> showDefault
         <> value 800
         <> metavar "INT")
      <*> option auto
          ( long "min-side-count"
         <> help "hex chessboard min side count"
         <> short '1'
         <> showDefault
         <> value 3
         <> metavar "INT")
      <*> option auto
          ( long "max-side-count"
         <> help "hex chessboard max side count"
         <> short '2'
         <> showDefault
         <> value 10
         <> metavar "INT")


main :: IO ()
main = genHexChessBoard =<< execParser opts
  where
    opts = info (inputs <**> helper)
      ( fullDesc
     <> progDesc "generates a hexagonal chessboard image using the mighty power of the Diagrams Lib (cue Yorgey Rap)"
     <> header "Hexagonal ChessBoard Image Generator " )


genHexChessBoard :: Inputs -> IO ()
genHexChessBoard (Inputs fileName w h cmin cmax) = let [wd, hd] = map fromIntegral [w, h] 
                                                       imageSize = dims2D wd hd :: SizeSpec V2 Double
                                                   in renderSVG fileName imageSize (genChessboard cmin cmax)
                                                                            
