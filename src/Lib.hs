{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


module Lib
    ( genChessboard
    ) where


import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD.Polygons


rotate ::Int ->  [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs 


rawPolygon :: Diagram B
rawPolygon =  polygon $ PolygonOpts (PolyRegular 6 4) OrientV origin


coloredPolygonList :: [Diagram B]
coloredPolygonList =  map (\x -> rawPolygon # fc x) [black, white, red]


topLeftHex :: Diagram B -> Diagram B
topLeftHex = snugT . snugL


topRightHex :: Diagram B -> Diagram B
topRightHex = snugT . snugR


xx :: Diagram B -> Diagram B
xx = snugL . alignB 


hexadd :: (Diagram B, Int) -> [Diagram B] -> (Diagram B, Int)
hexadd (mempty, 0) x = (xx . hcat $ x , length x) 
hexadd (top,pcount) bottom = let gradient = length bottom > pcount
                                 ff = if gradient 
                                         then topRightHex
                                         else topLeftHex                                        
                                 nbottom = hcat . map ff $ bottom
                             in ((top <> nbottom) # xx, length bottom)


genComponents :: Int -> Int -> [[Diagram B]]
genComponents minm maxm = let a = [minm .. maxm] ++ reverse [minm..maxm-1] 
                              b = [0 .. (maxm - minm)] 
                              c = b ++ (reverse . init $ b)
                          in map (\(x,y) -> take y . cycle $ Lib.rotate x coloredPolygonList) (zip c a)


genChessboard :: Int -> Int ->  Diagram B
genChessboard minB maxB = fst . foldl hexadd (mempty,0) $ genComponents minB maxB
