module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact

import Controls
import World

-- parameters and constants
windowWidth, windowHeight :: Int
windowWidth   = 800
windowHeight = 600

main :: IO ()
main = do
    -- window configs
    (wScreen, hScreen) <- getScreenSize
    let (wCenter, hCenter) = ((wScreen-windowWidth) `div` 2 , (hScreen-windowHeight) `div` 2)
    let disp = InWindow "FUNC INVADERS" (windowWidth, windowHeight) (wCenter,hCenter)
    -- game initial state
    let initialWorld = initiateGame
    play disp black 30 initialWorld photographWorld inputEvent updateAll