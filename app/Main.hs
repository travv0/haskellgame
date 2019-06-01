module Main where

import           Lib

import           Apecs
import           Apecs.Gloss

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    initialize
    play (InWindow "Shmup" (480, 360) (10, 10)) black 60 draw handleEvent step
