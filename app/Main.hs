{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Lib
import Diagrams.Prelude
import Diagrams.TwoD.Polygons
import Diagrams.Backend.SVG
import Diagrams.TwoD.Size


rotate ::Int ->  [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs 

sizE :: SizeSpec V2 Double
sizE = dims2D 800 800

rawPolygon :: Diagram B
rawPolygon =  polygon $  PolygonOpts (PolyRegular 6 4) OrientV origin

bp :: Diagram B
bp = rawPolygon # fc black 

wp :: Diagram B
wp = rawPolygon # fc white

rp :: Diagram B
rp = rawPolygon # fc red

cake :: Diagram B
cake = (bp ||| wp ||| rp) # showOrigin

t :: [Diagram B]
t =  [bp,wp,rp]


topLeftHex :: Diagram B -> Diagram B
topLeftHex x = x # snugL # snugT

topRightHex :: Diagram B -> Diagram B
topRightHex x = x # snugR # snugT


xx :: Diagram B -> Diagram B
xx = snugL . alignB 



hexadd :: (Diagram B, Int) -> [Diagram B] -> (Diagram B, Int)
hexadd (mempty, 0) x = ((hcat x) # xx # showOrigin, length x) 
hexadd (top,pcount) bottom = let gradient = (length bottom) > pcount
                                 ff = if gradient 
                                         then topRightHex
                                         else topLeftHex                                        
                                 nbottom = hcat . (map ff)$ bottom
                             in ((top <> nbottom) # xx, length bottom)


gengen :: Int -> Int -> [[Diagram B]]
gengen minm maxm = let a = [minm .. maxm] ++ (reverse [minm..maxm-1])
                       b = [0 .. (maxm - minm)]
                       c = b ++ (reverse . init $ b)
                   in map (\(x,y) -> (take y) . cycle $ Main.rotate x t) (zip c a)



egg :: Diagram B
egg = fst  (foldl hexadd (mempty,0) (gengen 2 12))

main :: IO ()
main = renderSVG "hello2.svg" sizE egg
