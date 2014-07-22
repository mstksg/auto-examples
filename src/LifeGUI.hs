module Main (main) where

import Graphics.Gloss

main :: IO ()
main = play (InWindow "Life" (1, 1) (500, 500))
            white
            1
            (0 :: Int)
            (\_   -> Blank )
            (\_ w -> w + 10)
            (\_ w -> w + 1 )
