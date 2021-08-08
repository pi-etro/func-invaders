module World where

import Graphics.Gloss
import Control.Parallel.Strategies
import System.Random

-- parameters and constants
cannonV, bulletV, rayV, alienV, ufoV, reloadTime, marchTime, rayTime, ufoTime :: Float
cannonV    = 200
bulletV    = 600
rayV       = -250
alienV     = 100
ufoV       = 100
reloadTime = 0.5
marchTime  = 0.5
rayTime    = 1
ufoTime    = 25

nRays :: Int
nRays      = 4

-- state machine
data WorldState = Start | Playing | Victory | Defeat
    deriving Eq

-- every game component (player, aliens, bullets, etc)
data Component a = Component { sprite :: Picture
                             , px     :: Float
                             , py     :: Float
                             , vx     :: Float
                             , vy     :: Float
                             , w      :: Float
                             , h      :: Float
                             , clock  :: Float
                             }

-- all the info of the game
data World a = World { state         :: WorldState
                     , rndGen        :: StdGen
                     , ufo           :: Component a
                     , aliens        :: [Component a]
                     , alienrays     :: [Component a]
                     , bunkers       :: [Component a]
                     , cannon        :: Component a
                     , cannonbullets :: [Component a]
                     , score         :: Int
                     , goLeft        :: Bool
                     , goRight       :: Bool
                     , shoot         :: Bool
                     , reload        :: Float
                     , loadray       :: Float
                     , parked        :: Float
                     }

-- convert the world a picture
photographWorld :: World a -> Picture
photographWorld world
    | state world == Start   = pictures [welcome_text, start_text, footer, score_text]
    | state world == Defeat  = pictures [game_over, restart_text, footer, score_text]
    | state world == Victory = pictures [you_win, restart_text, footer, score_text]
    | otherwise             = pictures $ cbullets ++ arays ++ [c, ovni, footer, score_text] ++ a ++ vaults
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
        arays    = parMap rpar pose $ alienrays world
        ovni     = pose $ ufo world
        vaults   = parMap rpar pose $ bunkers world
        -- pose component for the photo
        pose component = do
            let coordx = px component
            let coordy = py component
            --let width  = w component
            --let height  = h component
            Translate coordx coordy $ sprite component

-- initialize world
createWorld :: [Picture] -> StdGen -> World a
createWorld sprites gen =
    World
        Playing
        gen
        (Component Blank (-424) 272 0 0 48 21 0) -- ufo
        troop -- aliens
        [] -- alien rays
        vaults
        (Component (head sprites) 0 (-233) 0 0 45 24 0)  -- cannon
        [] -- cannon bullets
        0
        False
        False
        False
        1
        1
        0
    where
        troop = [Component (sprites!!4) (x*51.4) 227 alienV 0 24 24 0 | x <- [-5..5]] ++
                [Component (sprites!!3) (x*51.4) (227 - y*52.75) alienV 0 33 24 0 | x <- [-5..5], y <- [1..2]] ++
                [Component (sprites!!2) (x*51.4) (227 - y*52.75) alienV 0 36 24 0 | x <- [-5..5], y <- [3..4]]
        vaults = [Component (sprites!!7) (x*69) (-169) 0 0 69 48 0 | x <- [-3, -1, 1, 3]]

fireCannon :: [Picture] -> Float -> World a -> World a
fireCannon sprites t world
    | shoot world && reload world > reloadTime = world { cannonbullets = bullet : cannonbullets world, reload = 0 }
    | otherwise                                = world { reload = t + reload world }
    where
        bullet = Component (sprites!!1) x y 0 bulletV 3 18 0
        x = px $ cannon world
        y = 12 + py (cannon world)

shootLaser :: [Picture] -> Float -> World a -> World a
shootLaser sprites t world
    | length (alienrays world) >= nRays || loadray world < rayTime  = world { loadray = t + loadray world }
    | otherwise = world { alienrays = ray : alienrays world, loadray = 0 ,rndGen = snd $ rndTuple 10 (rndGen world)}
    where
        rndTuple :: Int -> StdGen -> (Int, StdGen)
        rndTuple m g = randomR (0, m) g
        rndElement g l = l !! fst (rndTuple (length l - 1) g)
        rshooter = rndElement (rndGen world)  $ aliens world
        x = px rshooter
        y = py rshooter
        ray = Component (sprites!!5) x y 0 rayV 9 21 0

-- update all components
update :: [Picture] -> StdGen -> Float -> World a -> World a
update sprites gen t world
    | state world == Playing = updateComponents
    | otherwise              = if shoot world then createWorld sprites gen else world
    where
        updateComponents = updateUfo sprites t
                         $ updateCannon sprites t
                         $ updateTroop sprites t
                         $ updateRays t
                         $ updateBullets t world

updatePosition :: Float -> Component a -> Component a
updatePosition t c@(Component _ x y v1 v2 _ _ clk)
    | clk > marchTime = c { px = x + t*v1, py = y + t*v2, clock = 0}
    | otherwise       = c { py = y + t*v2 , clock = clk + t}

-- update player position and fire
updateCannon :: [Picture] -> Float -> World a -> World a
updateCannon sprites t world = fireCannon sprites t world'
    where
        movement = if goRight world then t*cannonV else 0 + if goLeft world then t*(-cannonV) else 0
        margin = w (cannon world) / 2
        world' = world { cannon = (cannon world) { px = max (-375+margin) $ min (375-margin) $ movement + px (cannon world) } }

updateBullets :: Float -> World a -> World a
updateBullets t world = world {cannonbullets = filter (\x -> py x < 310) $ parMap rpar (updatePosition t) $ cannonbullets world}

updateTroop :: [Picture] -> Float -> World a -> World a
updateTroop sprites t world
    | null (aliens world) = world
    | otherwise = shootLaser sprites t world'
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
        world' = world { aliens = velocities }

updateRays :: Float -> World a -> World a
updateRays t world = world {alienrays = filter (\x -> py x > -256) $ parMap rpar (updatePosition t) $ alienrays world}

updateUfo :: [Picture] -> Float -> World a -> World a
updateUfo sprites t world
    | vx (ufo world) == 0 && parked world > ufoTime =
        if px (ufo world) < 0 then fly 1
        else fly (-1)
    | vx (ufo world) > 0 && px (ufo world) >= 424 = park
    | vx (ufo world) < 0 && px (ufo world) <= -424 = park
    | vx (ufo world) == 0 = world { parked = t + parked world }
    | otherwise = world { ufo = (ufo world) { px = px (ufo world) + t* vx (ufo world) } }
    where
        park = world { ufo = (ufo world) { sprite = Blank, vx = 0 }, parked = 0 }
        fly n = world { ufo = (ufo world) { sprite = sprites!!6, vx = n*ufoV } }