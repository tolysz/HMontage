
module Main where

import qualified Data.ByteString.Lazy as LB
import System.Environment( getArgs )
import Codec.Picture
import Codec.Picture.Types

import Filters.Scallers
import Control.Parallel.Strategies
import Control.Parallel

import Data.Either
import Control.Applicative
import Control.Arrow (first)

import Debug.Trace


thumbSize = 300

transformImage ::  Either String DynamicImage -> (Image PixelYCbCr8, (Double, Double))
transformImage (Right (ImageYCbCr8 img)) = scaleThumb thumbSize img -- Transform the image here
transformImage (Right (ImageY8 img ))  = trace     "Pixel8" $ first (convertImage . (promoteImage ::  Image Pixel8 -> Image PixelRGB8)) $ scaleThumb thumbSize img 
--transformImage (Right (ImageY16 img ))   = trace    "Pixel16" $ first (convertImage . (promoteImage ::  Image Pixel16 -> Image PixelRGB8)) $ scaleThumb thumbSize img 
--transformImage (Right (ImageYF img ))    = trace     "PixelF" $ first (convertImage . (promoteImage ::  Image PixelF -> Image PixelRGB8)) $ scaleThumb thumbSize img 
--transformImage (Right (ImageYA8 img ))   = trace    "PixelYA8" $ first (convertImage . (promoteImage ::  Image PixelYA8 -> Image PixelRGB8)) $ scaleThumb thumbSize img 
--transformImage (Right (ImageYA16 img ))  = trace   "PixelYA16" $ first (convertImage . (promoteImage ::  Image PixelYA16 -> Image PixelRGB8)) $ scaleThumb thumbSize img 
transformImage (Right (ImageRGB8 img ))  = trace   "PixelRGB8" $ first (convertImage ) $ scaleThumb thumbSize img 
--transformImage (Right (ImageRGB16 img )) = trace  "PixelRGB16" $ first (convertImage . (promoteImage ::  Image PixelRGB16 -> Image PixelRGB8)) $ scaleThumb thumbSize img 
--transformImage (Right (ImageRGBF img )) = trace   "PixelRGBF" $ first (convertImage . (promoteImage ::  Image PixelRGBF -> Image PixelRGB8)) $ scaleThumb thumbSize img 
--transformImage (Right (ImageRGBA8 img )) = trace  "PixelRGBA8" $ first (convertImage . (promoteImage ::  Image PixelRGBA8 -> Image PixelRGB8)) $ scaleThumb thumbSize img 
--transformImage (Right (ImageRGBA16 img )) = trace  "PixelRGBA16" $ first (convertImage . (promoteImage ::  Image PixelRGB16 -> Image PixelRGB8). (promoteImage ::  Image PixelRGBA16 -> Image PixelRGB16)) $ scaleThumb thumbSize img 
transformImage (Right (ImageCMYK8 img )) = trace   "PixelCMYK8" $ first (convertImage   . (convertImage ::  Image PixelCMYK8 -> Image PixelRGB8) ) $ scaleThumb thumbSize img 
--transformImage (Right (ImageCMYK16 img )) = trace  "PixelCMYK16" $ first (convertImage  . (promoteImage ::  Image PixelRGB16 -> Image PixelRGB8) . (convertImage ::  Image PixelCMYK16 -> Image PixelRGB16)) $ scaleThumb thumbSize img 
transformImage _ = emptyImage thumbSize
   -- error "file type not supported"



maP = parMap rseq
maPP = parMap rdeepseq

main :: IO ()
main = do
    commandArguments <- getArgs
    dynImgs <- mapM readImage commandArguments
    let ~cs =  maPP ( transformImage ) $ dynImgs
        -- (si,ll) = unzip ll2
        -- ~cs =  ll `using` parList rseq -- rdeepseq
        ~scales  = maP snd cs
        ~valid   = maP fst cs
        
    print $ scales
    LB.writeFile ("transformed.jpg") . encodeJpegAtQuality 95 $ concatImgs'' 15 thumbSize $ trace "*" ( valid ) --  `using` parList rdeepseq)

{-
    Right (ImageYCbCr8 img) ->
          writePng (filename ++ "_transformed.png") . transformRGBImage $ convertImage img
-}