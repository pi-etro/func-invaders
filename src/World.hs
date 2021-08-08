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
data Component a = Component { sprite  :: Picture
                             , px      :: Float
                             , py      :: Float
                             , vx      :: Float
                             , vy      :: Float
                             , w       :: Float
                             , h       :: Float
                             , clock   :: Float
                             , destroy :: Bool
                             , points  :: Int
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
                     , lives         :: Int
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
    | state world == Start   = pictures $ info ++ [welcome_text, start_text]
    | state world == Defeat  = pictures $ info ++ [game_over, restart_text]
    | state world == Victory = pictures $ info ++ [you_win, resume_text]
    | otherwise              = pictures $ info ++ game
    where
        -- screen elements and texts
        footer       = Color green $ Translate 0 (-262) $ rectangleSolid 750 3
        score_text   = Color white $ Translate (-350) (-290) $ Scale 0.16 0.16 $ Text $ "SCORE: " ++ show (score world)
        lives_text   = Color white $ Translate 265 (-290) $ Scale 0.16 0.16 $ Text $ "LIVES: " ++ show (lives world)
        welcome_text = Color green $ Translate (-105) 50 $ Scale 0.2 0.2 $ Text "FUNC INVADERS"
        start_text   = Color white $ Translate (-168) (-50) $ Scale 0.2 0.2 $ Text "PRESS SPACE TO START"
        restart_text = Color white $ Translate (-184) (-50) $ Scale 0.2 0.2 $ Text "PRESS SPACE TO RESTART"
        resume_text = Color white $ Translate (-180) (-50) $ Scale 0.2 0.2 $ Text "PRESS SPACE TO RESUME"
        game_over    = Color green $ Translate (-80) 50 $ Scale 0.2 0.2 $ Text "GAME OVER"
        you_win      = Color green $ Translate (-164) 50 $ Scale 0.2 0.2 $ Text "EARTH IS SAFE AGAIN !"
        info = [footer, score_text, lives_text]
        -- game components
        game = cbullets ++ arays ++ [c, ovni] ++ a ++ vaults
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
createWorld :: [Picture] -> StdGen -> Int -> Int -> WorldState -> World a
createWorld sprites gen s l stt =
    World
        stt
        gen
        (Component Blank (-424) 272 0 0 48 21 0 False 100) -- ufo
        troop -- aliens
        [] -- alien rays
        vaults
        (Component (head sprites) 0 (-233) 0 0 45 24 0 False 0)  -- cannon
        [] -- cannon bullets
        s
        l
        False
        False
        False
        1
        1
        0
    where
        troop = [Component (sprites!!4) (x*51.4) 227 alienV 0 24 24 0 False 30 | x <- [-5..5]] ++
                [Component (sprites!!3) (x*51.4) (227 - y*52.75) alienV 0 33 24 0 False 20 | x <- [-5..5], y <- [1..2]] ++
                [Component (sprites!!2) (x*51.4) (227 - y*52.75) alienV 0 36 24 0 False 10| x <- [-5..5], y <- [3..4]]
        vaults = [Component (sprites!!7) (x*69) (-169) 0 0 69 48 0 False 0 | x <- [-3, -1, 1, 3]]

fireCannon :: [Picture] -> Float -> World a -> World a
fireCannon sprites t world
    | shoot world && reload world > reloadTime = world { cannonbullets = bullet : cannonbullets world, reload = 0 }
    | otherwise                                = world { reload = t + reload world }
    where
        bullet = Component (sprites!!1) x y 0 bulletV 3 18 0 False 0
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
        ray = Component (sprites!!5) x y 0 rayV 9 21 0 False 0

-- update all components
update :: [Picture] -> StdGen -> Float -> World a -> World a
update sprites gen t world
    | state world == Playing = updateComponents
    | state world == Start   = if shoot world then world { state = Playing } else world
    | state world == Victory = if shoot world then createWorld sprites gen (score world) (lives world) Playing else world
    | otherwise              = if shoot world then createWorld sprites gen 0 3 Start else world
    where
        updateComponents = updateUfo sprites t
                         $ updateCannon sprites t
                         $ updateTroop sprites t
                         $ updateRays t
                         $ updateBullets t
                         $ updateDestroy
                         $ updateCollisions world

updatePosition :: Float -> Component a -> Component a
updatePosition t c@(Component _ x y v1 v2 _ _ clk _ _)
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
updateBullets t world = world {cannonbullets = filter (\x -> py x < 291) $ parMap rpar (updatePosition t) $ cannonbullets world}

updateTroop :: [Picture] -> Float -> World a -> World a
updateTroop sprites t world
    | null (aliens world) = world
    | ymin < -233 = world {state = Defeat}
    | otherwise = shootLaser sprites t world'
    where
        troop = aliens world
        dir = vx (head troop)
        posx = parMap rpar px troop
        ymin = minimum $ parMap rpar py troop
        xmax = maximum posx
        xmin = minimum posx
        moved = parMap rpar (updatePosition t) troop
        velocities
            | dir > 0 && xmax >= 357  = parMap rpar (\x -> x { py = py x-12, vx = -(vx x)}) moved
            | dir < 0 && xmin <= -357 = parMap rpar (\x -> x { py = py x-12, vx = -(vx x)}) moved
            | otherwise               = moved
        world' = world { aliens = velocities }

updateRays :: Float -> World a -> World a
updateRays t world = world {alienrays = filter (\x -> py x > -250) $ parMap rpar (updatePosition t) $ alienrays world}

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

collided :: Component a -> Component a -> Bool
collided (Component _ x1 y1 _ _ w1 h1 _ _ _ ) (Component _ x2 y2 _ _ w2 h2 _ _ _) = (fromleft || fromright) && (fromabove || frombelow)
    where
        fromleft = left2 < left1 && left1 < right2 -- 1 entering 2 from the left
        fromright = left2 < right1 && right1 < right2 -- 1 entering 2 from the right
        fromabove = bottom2 < bottom1 &&  bottom1 < top2 -- 1 entering 2 from the above
        frombelow = bottom2 < top1 &&  top1 < top2 -- 1 entering 2 from the below
        left1   = x1 - w1/2
        right1  = x1 + w1/2
        top1    = y1 + h1/2
        bottom1 = y1 - h1/2
        left2   = x2 - w2/2
        right2  = x2 + w2/2
        top2    = y2 + h2/2
        bottom2 = y2 - h2/2

checkCollisions :: [Component a] -> [Component a] -> ([Component a] , [Component a] )
checkCollisions [] c = ([], c)
checkCollisions c [] = (c, [])
checkCollisions (a:as) bs = ( a':as', bs'')
    where
        target comp l = parMap rpar (\x -> if collided comp x then x { destroy = True } else x) l
        bs' = target a bs
        a' = if any destroy bs' then a { destroy = True } else a
        (as', bs'') = checkCollisions as bs'

updateCollisions :: World a -> World a
updateCollisions world= world { ufo = head ufo'
                              , aliens = aliens'
                              , alienrays = rays''
                              , cannon = head cannon''
                              , cannonbullets = bullets'''}
                              --, bunkers = bunkers''}
    where
        (_, cannon')       = checkCollisions (aliens world) [cannon world]  -- aliens touched cannon
        (rays, cannon'')   = checkCollisions (alienrays world) cannon'  -- rays touched cannon
        (bullets, aliens') = checkCollisions (cannonbullets world) (aliens world) -- bullets touched aliens
        (bullets', ufo')   = checkCollisions bullets [ufo world] -- bullets touched ufo
        (bullets'', rays') = checkCollisions bullets' rays -- bullets touched rays possivel erro
        (bullets''', _)    = checkCollisions bullets'' (bunkers world) -- bullets touched bunkers
        (rays'', _)        = checkCollisions rays' (bunkers world) -- rays touched bunkers


updateDestroy :: World a -> World a
updateDestroy world= world {state = stt, ufo = u, aliens = aliens', alienrays = clear (alienrays world), cannonbullets = clear (cannonbullets world), bunkers = bunkers', cannon = cannon', score = score world + alienscore + ufoscore, lives = lives'}
    where
        clear l = filter (not . destroy) l
        alienscore = if stt == Defeat then 0 else sum $ parMap rpar (\x -> if destroy x then points x else 0) (aliens world)
        ufoscore = if destroy $ ufo world then points $ ufo world else 0
        u = if destroy $ ufo world then (ufo world) { sprite = Blank, destroy = False } else ufo world
        aliens' = clear (aliens world)
        bunkers' = parMap rpar (\x -> if destroy x then x {destroy = False} else x) (bunkers world)
        lives' = if destroy $ cannon world then lives world-1 else lives world
        cannon' = if destroy (cannon world) && lives' == 0 then cannon world else (cannon world) {destroy = False}
        stt
            | lives' <= 0 = Defeat
            | null aliens' = Victory
            | otherwise = Playing