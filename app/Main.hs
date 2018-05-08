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

--  ((wp # snugL # snugY (-1)) <> (bp # snugT))
--

data Hex = Hex {hdiagram :: Diagram B, hcount :: Int} 

hexadd :: Hex -> Hex -> Hex
hexadd (Hex mempty 0) x = x 
hexadd x (Hex mempty 0) = x
hexadd top bottom = let pre = if (hcount top) < (hcount bottom)
                                 then (\x -> x # alignL  # snugB # showOrigin)
                                 else (\x -> x # snugB # snugL # showOrigin)
                        bpre = if (hcount top) < (hcount bottom)
                                 then snugT
                                 else (snugT . snugL)
                        combinedD = (top # hdiagram # pre) <> (bottom # hdiagram # bpre)
                    in  Hex combinedD (hcount bottom)


gengen :: Int -> Int -> [[Diagram B]]
gengen minm maxm = let a = [minm .. maxm]  ++ (reverse [minm..maxm-1])
                       b = [0 .. (length a - 1)]
                       c = cycle t
                   in map (\(x,y) -> Main.rotate x $ take y c) (zip b a)


gengen2 :: [[Diagram B]] -> [Hex]
gengen2 = map (\x -> Hex (hcat x) (length x)) 

egg :: Diagram B
egg = hdiagram . (foldl hexadd (Hex mempty 0)) . gengen2 $ (gengen 1 4)

main :: IO ()
main = renderSVG "hello2.svg" sizE egg
