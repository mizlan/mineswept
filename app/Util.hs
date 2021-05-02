module Util (singleton, chunksOf) where

import qualified Data.Vector as V
import Data.Vector (Vector)

-- Polyfill?
singleton :: a -> [a]
singleton a = [a]

chunksOf :: Int -> Vector a -> Vector (Vector a)
chunksOf n v
  | null v = V.empty
  | otherwise =
    let (f, r) = V.splitAt n v
     in f `V.cons` chunksOf n r
