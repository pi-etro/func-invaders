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
    cannonBMP    <- loadBMP "resources/cannon.bmp"
    bulletBMP    <- loadBMP "resources/bullet.bmp"
    mrcrabs   <- loadBMP "resources/crab.bmp"
    octopus   <- loadBMP "resources/octopus.bmp"
    squidward <- loadBMP "resources/squid.bmp"
    ray       <- loadBMP "resources/ray.bmp"
    ufo       <- loadBMP "resources/ufo.bmp"
    bunker    <- loadBMP "resources/bunker.bmp"
    let sprites = [cannonBMP, bulletBMP, mrcrabs, octopus, squidward, ray, ufo, bunker]
    -- game initial state
    play
        disp
        black
        fps
        (createWorld sprites)
        photographWorld
        controlEvent
        (update sprites)