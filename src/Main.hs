module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact

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

-- state machine
data GameState = Start | Playing | Victory | Defeat
    deriving Eq

-- every game component (player, aliens, bullets, etc)
data Component a = Component { px :: Float
                             , py :: Float
                             , vx :: Float
                             , vy :: Float
                             , w  :: Float
                             , h  :: Float
                             }

-- all the info of the game
data GameData a = GameData { state         :: GameState
--                           , ufo          :: [Component a]
--                           , aliens        :: [Component a]
--                           , aliensbullets :: [Component a]
--                           , bunkers       :: [Component a]
                           , player        :: Component a
--                           , playerbullets :: [Component a]
                           , score         :: Int
                           , goLeft        :: Bool
                           , goRight       :: Bool
                           , shoot         :: Bool
                           }

-- convert the world a picture
photographWorld :: GameData a -> Picture
photographWorld game
    | state game == Start   = pictures [welcome_text, start_text, footer, score_text]
    | state game == Defeat  = pictures [game_over, restart_text, footer, score_text]
    | state game == Victory = pictures [you_win, restart_text, footer, score_text]
    | otherwise             = pictures [canon, footer, score_text]
    where
        footer       = Color green $ Translate 0 (-260) $ rectangleSolid 750 3
        score_text   = Color white $ Translate (-350) (-290) $ Scale 0.16 0.16 $ Text $ "SCORE: " ++ show (score game)
        welcome_text = Color green $ Translate (-105) 50 $ Scale 0.2 0.2 $ Text "FUNC INVADERS"
        start_text   = Color white $ Translate (-168) (-50) $ Scale 0.2 0.2 $ Text "PRESS SPACE TO START"
        restart_text = Color white $ Translate (-184) (-50) $ Scale 0.2 0.2 $ Text "PRESS SPACE TO RESTART"
        game_over    = Color green $ Translate (-80) 50 $ Scale 0.2 0.2 $ Text "GAME OVER"
        you_win      = Color green $ Translate (-164) 50 $ Scale 0.2 0.2 $ Text "EARTH IS SAFE AGAIN !"
--        enemies     = pose green (aliens game)
--        ebullets     = pose green (aliensbullets game)
        canon        = pose green (player game)
--        cbullets     = pose green (playerbullets game)
        pose clr component = do
            let coordx = px component
            let coordy = py component
            let width  = w component
            let height  = h component
            Color clr $ Translate coordx coordy $ rectangleSolid width height


-- handle input events
inputEvent :: Event -> GameData a -> GameData a
-- space to confirm and fire
inputEvent (EventKey (SpecialKey KeySpace) Down _ _) game = game { shoot = True }
inputEvent (EventKey (SpecialKey KeySpace) Up _ _) game   = game { shoot = False }
-- left arrow to go left
inputEvent (EventKey (SpecialKey KeyLeft) Down _ _) game  = game { goLeft = True }
inputEvent (EventKey (SpecialKey KeyLeft) Up _ _) game    = game { goLeft = False }
-- left arrow to go right
inputEvent (EventKey (SpecialKey KeyRight) Down _ _) game = game { goRight = True }
inputEvent (EventKey (SpecialKey KeyRight) Up _ _) game   = game { goRight = False }
-- letter a to go left
inputEvent (EventKey (Char 'a') Down _ _) game            = game { goLeft = True }
inputEvent (EventKey (Char 'a') Up _ _) game              = game { goLeft = False }
-- letter d to go right
inputEvent (EventKey (Char 'd')  Down _ _) game           = game { goRight = True }
inputEvent (EventKey (Char 'd')  Up _ _) game             = game { goRight = False }
inputEvent _ game = game

-- initialize world
initiateGame :: GameData a
initiateGame = GameData Playing canon 0 False False False
    where
        canon = Component 0 (-218) 0 0 60 20

-- update all components
updateAll :: Float -> GameData a -> GameData a
updateAll t game
    | state game == Playing = updateComponents t game
    | otherwise             = if shoot game then initiateGame else game
    where updateComponents t' game' = updatePlayer t' game'

-- update player position and fire
updatePlayer :: Float -> GameData a -> GameData a
updatePlayer t game = game'
    where
        deslocamento = if goRight game then t*300 else 0 + if goLeft game then t*(-300) else 0
        borda = w (player game) / 2
        game' = game { player = (player game) { px = max (-375+borda) $ min (375-borda) $ deslocamento + px (player game)}}