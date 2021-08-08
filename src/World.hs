module World where

import Graphics.Gloss
import Control.Parallel.Strategies

-- parameters and constants
cannonV, bulletV, alienV, reloadTime :: Float
cannonV    = 200
bulletV    = 600
alienV     = 15
reloadTime = 0.5

-- state machine
data WorldState = Start | Playing | Victory | Defeat
    deriving Eq

-- every game component (player, aliens, bullets, etc)
data Component a = Component { sprite :: Picture
                             , px :: Float
                             , py :: Float
                             , vx :: Float
                             , vy :: Float
                             , w  :: Float
                             , h  :: Float
                             }

-- all the info of the game
data World a = World { state         :: WorldState
                     --, ufo          :: [Component a]
                     , aliens        :: [Component a]
                     --, aliensbullets :: [Component a]
                     --, bunkers       :: [Component a]
                     , cannon        :: Component a
                     , cannonbullets  :: [Component a]
                     , score         :: Int
                     , goLeft        :: Bool
                     , goRight       :: Bool
                     , shoot         :: Bool
                     , reload        :: Float
                     }

-- convert the world a picture
photographWorld :: World a -> Picture
photographWorld world
    | state world == Start   = pictures [welcome_text, start_text, footer, score_text]
    | state world == Defeat  = pictures [game_over, restart_text, footer, score_text]
    | state world == Victory = pictures [you_win, restart_text, footer, score_text]
    | otherwise             = pictures $ [c, footer, score_text] ++ cbullets ++ a
    where
        -- screen elements and texts
        footer       = Color green $ Translate 0 (-262) $ rectangleSolid 750 3
        score_text   = Color white $ Translate (-350) (-290) $ Scale 0.16 0.16 $ Text $ "SCORE: " ++ show (score world)
        welcome_text = Color green $ Translate (-105) 50 $ Scale 0.2 0.2 $ Text "FUNC INVADERS"
        start_text   = Color white $ Translate (-168) (-50) $ Scale 0.2 0.2 $ Text "PRESS SPACE TO START"
        restart_text = Color white $ Translate (-184) (-50) $ Scale 0.2 0.2 $ Text "PRESS SPACE TO RESTART"
        game_over    = Color green $ Translate (-80) 50 $ Scale 0.2 0.2 $ Text "GAME OVER"
        you_win      = Color green $ Translate (-164) 50 $ Scale 0.2 0.2 $ Text "EARTH IS SAFE AGAIN !"
        -- game components
        c        = pose $ cannon world
        cbullets = parMap rpar pose $ cannonbullets world
        a        = parMap rpar pose $ aliens world
        -- pose component for the photo
        pose component = do
            let coordx = px component
            let coordy = py component
            --let width  = w component
            --let height  = h component
            Translate coordx coordy $ sprite component

-- initialize world
createWorld :: [Picture] -> World a
createWorld sprites =
    World
        Playing
        troop -- aliens
        (Component (head sprites) 0 (-233) 0 0 45 24)  -- cannon
        [] -- cannon bullets
        0
        False
        False
        False
        1
    where
        troop = [Component (sprites!!4) (x*51.4) 227 alienV 0 24 24 | x <- [-5..5]] ++
                [Component (sprites!!3) (x*51.4) (227 - y*52.75) alienV 0 33 24 | x <- [-5..5], y <- [1..2]] ++
                [Component (sprites!!2) (x*51.4) (227 - y*52.75) alienV 0 36 24 | x <- [-5..5], y <- [3..4]]

fireCannon :: [Picture] -> Float -> World a -> World a
fireCannon sprites t world
    | shoot world && reload world > reloadTime = world { cannonbullets = bullet : cannonbullets world, reload = 0 }
    | otherwise                                = world { reload = t + reload world }
    where
        bullet = Component (sprites!!1) x y 0 bulletV 3 18
        x = px $ cannon world
        y = 12 + py (cannon world)

--shootLaser :: [Picture] -> Float -> World a -> World a

-- update all components
update :: [Picture] -> Float -> World a -> World a
update sprites t world
    | state world == Playing = updateComponents
    | otherwise              = if shoot world then createWorld sprites else world
    where
        updateComponents = updateCannon sprites t $ updateTroop t $updateBullets t world

updatePosition :: Float -> Component a -> Component a
updatePosition t c@(Component _ x y v1 v2 _ _ ) = c { px = x + t*v1, py = y + t*v2 }

-- update player position and fire
updateCannon :: [Picture] -> Float -> World a -> World a
updateCannon sprites t world = fireCannon sprites t world'
    where
        movement = if goRight world then t*cannonV else 0 + if goLeft world then t*(-cannonV) else 0
        margin = w (cannon world) / 2
        world' = world { cannon = (cannon world) { px = max (-375+margin) $ min (375-margin) $ movement + px (cannon world) } }

updateBullets :: Float -> World a -> World a
updateBullets t world = world {cannonbullets = filter (\x -> py x < 310) $ parMap rpar (updatePosition t) $ cannonbullets world}

updateTroop :: Float -> World a -> World a
updateTroop t world
    | null (aliens world) = world
    | otherwise = world { aliens = velocities }
    where
        troop = aliens world
        dir = vx (head troop)
        posx = parMap rpar px troop
        xmax = maximum posx
        xmin = minimum posx
        moved = parMap rpar (updatePosition t) troop
        velocities
            | dir > 0 && xmax >= 357  = parMap rpar (\x -> x { py = py x-12, vx = -(vx x)}) moved
            | dir < 0 && xmin <= -357 = parMap rpar (\x -> x { py = py x-12, vx = -(vx x)}) moved
            | otherwise               = moved

