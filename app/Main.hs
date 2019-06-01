module Main where

import           Lib

import           Apecs
import           Apecs.Gloss
import           Control.Monad

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    initialize
    play (InWindow "Shmup" (220, 360) (10, 10))
         black
         60
         draw
         (handleInput >=> handleEvent)
         step
