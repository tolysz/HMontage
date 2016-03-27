{-# Language ViewPatterns
           , RankNTypes
           , ScopedTypeVariables
           #-}

module Filters.Scallers
 ( scaleThumb
 , emptyImage
 , concatImgs
 , concatImgs'
 , concatImgs''
 ) where

import Codec.Picture
import Codec.Picture.Types
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Control.Monad.ST( runST )

import qualified Data.List as DL

import Debug.Trace

scaleThumb :: Pixel a => Int -> Image a -> (Image a, (Double, Double))
scaleThumb i img@(Image w h _) = (generateImage (rscale img ratioX ratioY) i i, (ratioX,ratioY) )  -- Transform the image here
  where
  	fi = fromIntegral i
  	(ratioX,ratioY) = ( fromIntegral w / fi , fromIntegral h / fi)

--rscale :: Pixel a => Image a -> Int -> (Int -> Int -> Pixel a) 
rscale im ratioX ratioY = \(fromIntegral -> x) (fromIntegral -> y) -> pixelAt im (round $ ratioX * x) (round $ ratioY * y)

emptyImage i = (generateImage (\_ _ -> emptyPixYCbCr8 ) i i , (1 ,1))

emptyPixYCbCr8  = PixelYCbCr8 0 0 0

concatImgs row thbSize ls = generateImage comb cx cy
  where
    fd = flip divMod thbSize
    (full_lines,rest) = ((length ls) ) `divMod` row
    (cx,cy) = case (full_lines,rest) of
           (0, a) -> (a * thbSize, thbSize)
           (b, 0) -> (row * thbSize, b * thbSize)
           (b, _) -> (row * thbSize, (b + 1) * thbSize)

    comb (fd -> (px, x)) (fd -> (py, y)) 
            | (py < full_lines || px < rest) = pixelAt (ls !! (px + (row * py)) ) x y
    	    | otherwise                      = emptyPixYCbCr8



concatImgs' :: forall a. (Pixel a)
              => Int -- images pre row
              -> Int -- thumb size
              -> [ Image a ]-- Images to concat (Int -> Int -> a)  -- ^ Generating function, with `x` and `y` params.
              -> Image a
{-# INLINE concatImgs' #-}
concatImgs' row thbSize ls = Image { imageWidth = w, imageHeight = h, imageData = generated }
  where compCount = componentCount (undefined :: a)
        fd = flip divMod thbSize
        (full_lines,rest) = ((length ls) ) `divMod` row
        (w,h) = case (full_lines,rest) of
           (0, a) -> (a * thbSize, thbSize)
           (b, 0) -> (row * thbSize, b * thbSize)
           (b, _) -> (row * thbSize, (b + 1) * thbSize)
                 --    = emptyPixYCbCr8

        generated = runST $ do
            arr <- M.new (w * h * compCount)
            let comb arr idx (fd -> (px, x)) (fd -> (py, y)) 
                    | (py < full_lines || px < rest) = unsafeWritePixel arr idx $ pixelAt (ls !! (px + (row * py)) ) x y
    	            | otherwise = return ()
                lineGenerator _ y | y >= h = return ()
                lineGenerator lineIdx y = column lineIdx 0
                  where column idx x | x >= w = lineGenerator idx $ y + 1
                        column idx x = do
                            comb arr idx x y
                            column (idx + compCount) $ x + 1

            lineGenerator 0 0
            V.unsafeFreeze arr


concatImgs'' :: forall a. (Pixel a)
              => Int -- images pre row
              -> Int -- thumb size
              -> [ Image a ]-- Images to concat (Int -> Int -> a)  -- ^ Generating function, with `x` and `y` params.
              -> Image a
{-# INLINE concatImgs'' #-}
concatImgs'' row thbSize ls = Image { imageWidth = w, imageHeight = h, imageData = generated }
  where compCount = componentCount (undefined :: a)
        fd = flip divMod thbSize
        vsize = compCount * thbSize
        lsFill = ls ++ (replicate pad emptyPix)
        (full_lines,rest) = ((length ls) ) `divMod` row
        (w,h, pad) = case (full_lines,rest) of
           (0, a) -> (a * thbSize, thbSize , 0)
           (b, 0) -> (row * thbSize, b * thbSize, 0)
           (b, a) -> (row * thbSize, (b + 1) * thbSize, row - a)
        emptyPix :: Image a
        emptyPix = Image thbSize thbSize $ runST $ V.unsafeFreeze =<< M.new (thbSize * thbSize * compCount)
  
        generated = V.concat $ someMagic $ map (\(Image _ _ v) -> v) lsFill

        someMagic [] = []
        someMagic xls = moreMagic (take row xls) 0 ++ someMagic (drop row xls)

        moreMagic xls c
           | c == (thbSize * vsize) = []
           | otherwise    = (map (V.slice c vsize) xls) ++ moreMagic xls (c + vsize) ---  (map (V.drop vsize) xls)  
