module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
--import Graphics.Gloss.Interface.IO.Interact

import Controls
import World

-- parameters and constants
windowWidth, windowHeight, fps :: Int
windowWidth   = 800
windowHeight  = 600
fps           = 30

main :: IO ()
main = do
    -- window configs
    (wScreen, hScreen) <- getScreenSize
    let (wCenter, hCenter) = ((wScreen-windowWidth) `div` 2 , (hScreen-windowHeight) `div` 2)
    let disp = InWindow "FUNC INVADERS" (windowWidth, windowHeight) (wCenter,hCenter)
    -- sprites
    cannon <- loadBMP "resources/cannon.bmp"
    --Bitmap mrcrabs   <- loadBMP "resources/crab.bmp"
    --Bitmap octopus   <- loadBMP "resources/octopus.bmp"
    --Bitmap squidward <- loadBMP "resources/squid.bmp"
    --Bitmap ufo       <- loadBMP "resources/ufo.bmp"
    -- game initial state
    let initialWorld = initiateGame
    play
        disp
        black
        fps
        (initialWorld cannon)
        photographWorld
        inputEvent
        (updateAll cannon)