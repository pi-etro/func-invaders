module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
--import Graphics.Gloss.Interface.IO.Interact
import System.Random

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
    cannonBMP      <- loadBMP "resources/cannon.bmp"
    bulletBMP      <- loadBMP "resources/bullet.bmp"
    octopus        <- loadBMP "resources/octopus.bmp"
    octopus_alt    <- loadBMP "resources/octopus_alt.bmp"
    crab           <- loadBMP "resources/crab.bmp"
    crab_alt       <- loadBMP "resources/crab_alt.bmp"
    squid          <- loadBMP "resources/squid.bmp"
    squid_alt      <- loadBMP "resources/squid_alt.bmp"
    ray            <- loadBMP "resources/ray.bmp"
    spaceship      <- loadBMP "resources/ufo.bmp"
    explosion      <- loadBMP "resources/alien_explosion.bmp"
    ship_explosion <- loadBMP "resources/ufo_explosion.bmp"
    bunker         <- loadBMP "resources/bunker.bmp"
    let sprites = [ 
                    cannonBMP
                  , bulletBMP
                  , octopus
                  , crab
                  , squid
                  , ray
                  , spaceship
                  , bunker
                  , crab_alt
                  , octopus_alt
                  , squid_alt
                  , explosion
                  , ship_explosion
                  ]
    -- random generator
    g <- newStdGen
    -- game initial state
    play
        disp
        black
        fps
        (createWorld sprites g 0 3 Start)
        photographWorld
        controlEvent
        (update sprites g)