
module Montage.Types where

import Data.HashMap.Strict (Map)
import qualified Data.HashMap.Strict as Map

newtype Meta a
  = Meta
    { org     :: Dimms
    , stored  :: Dimms
    , rotate  :: Int
    , meta    :: a 
    }

newtype Dimms
  = Dimms
    { width  :: Int
    , height :: Int
    } 

newtype PosXY 
  = PosXY
    { x :: Int
    , y :: Int
    } 

data ImageBlob a ix 
  = ImageBlob
    { tileSize     :: Dimms
    , tilesPerLine :: Int
    , newFree      :: Int
    , blob         :: MMaped File
    , stored       :: Map Int (Meta a)
    , index        :: Map ix Int
    }	

-- nubmer of picture picture to its meta data
{-----
  | Image 1.1 | Image 1.2 | 
  | Image 2.1 | Image 2.2 |


-- a :: * -> *
--}

--putImage 
