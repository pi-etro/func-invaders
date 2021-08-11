module World where

-- libraries
import Graphics.Gloss
    ( Picture(Blank, Color, Scale, Text, Translate),
      green,
      white,
      pictures,
      rectangleSolid )
import Control.Parallel.Strategies ( parMap, rpar )
import System.Random ( StdGen, Random(randomR) )

-- parameters
cannonV, bulletV, rayV, ufoV, reloadTime, rayTime, ufoTime :: Float
cannonV    = 200  -- cannon movement speed
bulletV    = 600  -- cannon bullets speed
reloadTime = 0.5  -- limit cannon rate of fire
ufoV       = 100  -- ufo flying speed
ufoTime    = 25   -- ufo time interval
rayV       = -250 -- aliens ray beam speed
rayTime    = 1    -- time to spawn a ray beam

-- parameters based on current number of aliens
-- maximum number of simultaneous ray beams (linear from 5 to 3 when only one alien exists)
nRays :: Int -> Int
nRays n = (subtract 3 5 `div` 54) * (n-1) + 3

-- aliens march speed (linear from 100 to 150 when only one alien exists)
alienV :: Int -> Float
alienV n = (subtract 150 100/54) * fromIntegral (n-1) + 150

-- aliens march rhythm (linear from 0.5 to 0.1 when only one alien exists)
marchTime :: Int -> Float
marchTime n = (subtract 0.1 0.5/54) * fromIntegral (n-1) + 0.1

-- world state machine
data WorldState = Start | Playing | Victory | Defeat
    deriving Eq

-- game component (player, items, npcs)
data Component a = Component {
                               sprite   :: Picture -- component sprite
                             , spriteid :: Int     -- sprite index
                             , px       :: Float   -- x-axis position
                             , py       :: Float   -- y-axis position
                             , vx       :: Float   -- x-axis velocity
                             , vy       :: Float   -- y-axis velocity
                             , w        :: Float   -- width
                             , h        :: Float   -- height
                             , clock    :: Float   -- timer used on invaders troops
                             , destroy  :: Bool    -- destroy flag
                             , points   :: Int     -- points to add to the score
                             }

-- all the info of the game
data World a = World {
                       state         :: WorldState    -- current world state
                     , rndGen        :: StdGen        -- pseudo-random number generator
                     , ufo           :: Component a
                     , aliens        :: [Component a] -- alive aliens list
                     , alienrays     :: [Component a] -- list of fired ray beams
                     , bunkers       :: [Component a]
                     , cannon        :: Component a
                     , cannonbullets :: [Component a] -- list of fired bullets
                     , score         :: Int           -- current score
                     , hscore        :: Int           -- current high score
                     , lives         :: Int           -- current lives
                     , goLeft        :: Bool          -- flag to turn cannon left
                     , goRight       :: Bool          -- flag to turn cannon right
                     , shoot         :: Bool          -- flag to fire cannon
                     , reload        :: Float         -- current cannon reloading time
                     , loadray       :: Float         -- current ray gun reloading time
                     , parked        :: Float         -- elapsed UFO parked time
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
        info = [footer, score_text, hscore_text] ++ liv
        footer       = Color green $ Translate 0 (-262) $ rectangleSolid 750 3
        hscore_text  = Color white $ Translate (-105) (-290) $ Scale 0.16 0.16 $ Text $ "HI-SCORE: " ++ show (hscore world)
        score_text   = Color white $ Translate (-350) (-290) $ Scale 0.16 0.16 $ Text $ "SCORE: " ++ show (score world)
        welcome_text = Color green $ Translate (-105) 50 $ Scale 0.2 0.2 $ Text "FUNC INVADERS"
        start_text   = Color white $ Translate (-168) (-50) $ Scale 0.2 0.2 $ Text "PRESS SPACE TO START"
        restart_text = Color white $ Translate (-184) (-50) $ Scale 0.2 0.2 $ Text "PRESS SPACE TO RESTART"
        resume_text  = Color white $ Translate (-180) (-50) $ Scale 0.2 0.2 $ Text "PRESS SPACE TO RESUME"
        game_over    = Color green $ Translate (-80) 50 $ Scale 0.2 0.2 $ Text "GAME OVER"
        you_win      = Color green $ Translate (-164) 50 $ Scale 0.2 0.2 $ Text "EARTH IS SAFE AGAIN !"
        liv = parMap rpar pose $ [Component c_icon 0 (fromIntegral x *65 + 197) (-282) 0 0 45 24 0 False 0 | x <- [0 .. lives world - 1]]
        c_icon = sprite $ cannon world

        -- game components
        game = cb ++ ar ++ [c, u] ++ a ++ b
        c  = pose $ cannon world
        u  = pose $ ufo world
        cb = parMap rpar pose $ cannonbullets world
        a  = parMap rpar pose $ aliens world
        ar = parMap rpar pose $ alienrays world
        b  = parMap rpar pose $ bunkers world

        -- prepares the game components for the photo
        pose component = do
            let coordx = px component
            let coordy = py component
            Translate coordx coordy $ sprite component

-- initialize world
createWorld :: [Picture] -> StdGen -> Int -> Int -> Int -> WorldState -> World a
createWorld sprites gen scr hscr liv stt =
    World
        stt                                                       -- world state
        gen                                                       -- random generator
        (Component Blank 6 (-424) 272 0 0 48 21 0 False 100)      -- ufo
        invaders                                                  -- aliens
        []                                                        -- alien rays
        vaults                                                    -- bunkers
        (Component (head sprites) 0 0 (-233) 0 0 45 24 0 False 0) -- cannon
        []                                                        -- cannon bullets
        scr                                                       -- score
        hscr                                                      -- high score
        liv                                                       -- lives
        False                                                     -- go left
        False                                                     -- go right
        False                                                     -- shoot
        0                                                         -- disble fire on start
        0                                                         -- disable rays on start
        0                                                         -- ufo starts parked
    where
        -- squids, crabs and octopus
        invaders =
            [Component (sprites!!4) 4 (x*51.4)  227            (alienV 55) 0 24 24 0 False 30 | x <- [-5..5]] ++
            [Component (sprites!!3) 3 (x*51.4) (227 - y*52.75) (alienV 55) 0 33 24 0 False 20 | x <- [-5..5], y <- [1..2]] ++
            [Component (sprites!!2) 2 (x*51.4) (227 - y*52.75) (alienV 55) 0 36 24 0 False 10 | x <- [-5..5], y <- [3..4]]
        -- bunkers
        vaults = [Component (sprites!!7) 7 (x*69) (-169) 0 0 69 48 0 False 0 | x <- [-3, -1, 1, 3]]

-- spawn a bullet on shoot command if not reloading
fireCannon :: [Picture] -> Float -> World a -> World a
fireCannon sprites t world
    | shoot world && reload world > reloadTime = world { cannonbullets = bullet : cannonbullets world, reload = 0 }
    | otherwise                                = world { reload = t + reload world }
    where
        bullet = Component (sprites!!1) 1 x y 0 bulletV 3 18 0 False 0
        x = px $ cannon world
        y = 12 + py (cannon world)

-- spawn a new ray beam if not reloading and number of rays is lower than limit
shootLaser :: [Picture] -> Float -> World a -> World a
shootLaser sprites t world
    | length (alienrays world) >= nRays (length $ aliens world) || loadray world < rayTime  = world { loadray = t + loadray world }
    | otherwise                                                                             = world {
                                                                                                      alienrays = ray : alienrays world
                                                                                                     , loadray = 0
                                                                                                     , rndGen = snd $ rndTuple 11 (rndGen world) }
    where
        ray = Component (sprites!!5) 5 x y 0 rayV 9 21 0 False 0
        x = px rndshooter
        y = py rndshooter
        rndshooter = rndElement (rndGen world)  $ aliens world -- selects a random alien to fire
        rndElement g l = l !! fst (rndTuple (length l - 1) g)
        rndTuple :: Int -> StdGen -> (Int, StdGen)
        rndTuple m g = randomR (0, m) g

-- control state machine and update all components
update :: [Picture] -> StdGen -> Float -> World a -> World a
update sprites gen t world
    | state world == Playing = updateComponents
    | state world == Start   = if shoot world then world { state = Playing } else world
    | state world == Victory = if shoot world then createWorld sprites gen (score world) (hscore world) (lives world) Playing else world
    | otherwise              = if shoot world then createWorld sprites gen 0 maxscr 3 Start else world
    where
        maxscr = if score world > hscore world then score world else hscore world
        updateComponents = updateUfo sprites t
                         $ updateCannon sprites t
                         $ updateTroop sprites t
                         $ updateRays t
                         $ updateBullets t
                         $ updateDestroy sprites t
                         $ updateCollisions world

-- used to update bullets, rays and aliens positions ans sprites
updatePosition :: [Picture] -> Int -> Float -> Component a -> Component a
updatePosition sprites naliens t c@(Component sprt sid x y v1 v2 _ _ clk _ _)
    | clk > marchTime naliens = c { sprite = invsprt
                                  , spriteid = invid
                                  , px = x + t*v1
                                  , py = y + t*v2
                                  , vx = compvx
                                  , clock = 0
                                  , destroy = sid < 0
                                  }
    | otherwise               = c { py = y + t*v2
                                  , clock = clk + t }
    where
        compvx
            | sid == 1 || sid == 5 = 0
            | otherwise            = if v1 > 0 then alienV naliens else -(alienV naliens)
        invsprt
            | null sprites || sid < 0 = sprt
            | otherwise               = sprites!!invid
        invid
            | sid >= 2 && sid <= 4  = sid + 6
            | sid >= 8 && sid <= 10 = sid - 6
            | otherwise             = sid

-- update cannon position and fire bullets
updateCannon :: [Picture] -> Float -> World a -> World a
updateCannon sprites t world = fireCannon sprites t world'
    where
        movement
            | goRight world = t*cannonV
            | goLeft world  = t*(-cannonV)
            | otherwise     = 0
        margin = w (cannon world) / 2
        world' = world { cannon = (cannon world) { px = max (-375+margin) $ min (375-margin) $ movement + px (cannon world) } }

-- update bullets positions
updateBullets :: Float -> World a -> World a
updateBullets t world = world {cannonbullets = filter (\x -> py x < 291) $ parMap rpar (updatePosition [] (length $ aliens world) t) $ cannonbullets world}

-- update aliens positions and fire ray beams
updateTroop :: [Picture] -> Float -> World a -> World a
updateTroop sprites t world
    | null (aliens world) = world
    | ymin < -233         = world { state = Defeat } -- aliens landed on earth
    | otherwise           = shootLaser sprites t world'
    where
        world' = world { aliens = velocities }
        velocities
            | dir > 0 && xmax >= 357  = parMap rpar (\x -> x { py = py x-12, vx = -(vx x)}) moved
            | dir < 0 && xmin <= -357 = parMap rpar (\x -> x { py = py x-12, vx = -(vx x)}) moved
            | otherwise               = moved
        dir = vx (head troop)
        moved = parMap rpar (updatePosition sprites (length $ aliens world) t) troop
        troop = aliens world
        ymin = minimum $ parMap rpar py troop
        xmax = maximum posx
        xmin = minimum posx
        posx = parMap rpar px troop

-- update ray beams positions
updateRays :: Float -> World a -> World a
updateRays t world = world {alienrays = filter (\x -> py x > -250) $ parMap rpar (updatePosition [] (length $ aliens world) t) $ alienrays world}

-- update UFO position
updateUfo :: [Picture] -> Float -> World a -> World a
updateUfo sprites t world
    | vx (ufo world) == 0 && parked world > ufoTime = if px (ufo world) < 0 then fly 1 else fly (-1)
    | vx (ufo world) > 0 && px (ufo world) >= 424   = park
    | vx (ufo world) < 0 && px (ufo world) <= -424  = park
    | vx (ufo world) == 0                           = world { parked = t + parked world }
    | otherwise                                     = world { ufo = (ufo world) { px = px (ufo world) + t*vx (ufo world) } }
    where
        park = world { ufo = (ufo world) { sprite = Blank, vx = 0 }, parked = 0 }
        fly n = world { ufo = (ufo world) { sprite = sprites!!6, vx = n*ufoV, destroy = False, points = 100 } }

-- check if two components collided
collided :: Component a -> Component a -> Bool
collided (Component _ _ x1 y1 _ _ w1 h1 _ _ _ ) (Component _ _ x2 y2 _ _ w2 h2 _ _ _) =
    (fromleft || fromright) && (fromabove || frombelow)
    where
        fromleft = left2 < left1 && left1 < right2       -- 1 entering 2 from the left
        fromright = left2 < right1 && right1 < right2    -- 1 entering 2 from the right
        fromabove = bottom2 < bottom1 &&  bottom1 < top2 -- 1 entering 2 from above
        frombelow = bottom2 < top1 &&  top1 < top2       -- 1 entering 2 from below
        left1   = x1 - w1/2
        right1  = x1 + w1/2
        top1    = y1 + h1/2
        bottom1 = y1 - h1/2
        left2   = x2 - w2/2
        right2  = x2 + w2/2
        top2    = y2 + h2/2
        bottom2 = y2 - h2/2

-- check if components from lists collided
checkCollisions :: [Component a] -> [Component a] -> ([Component a] , [Component a] )
checkCollisions [] c = ([], c)
checkCollisions c [] = (c, [])
checkCollisions (a:as) bs = ( a':as', bs'')
    where
        bs' = parMap rpar (\x -> if collided a x then x { destroy = True } else x) bs -- activate destroy flags
        a'
            | hit > 0   = a { destroy = True }
            | otherwise = a
        (as', bs'') = checkCollisions as bs'
        hit :: Int
        hit =  sum $ parMap rpar (\x -> if collided a x then 1 else 0) bs

-- update components with new list with destroy flags
updateCollisions :: World a -> World a
updateCollisions world= world {
                                ufo = head ufo'
                              , aliens = aliens'
                              , alienrays = rays''
                              , cannon = head cannon''
                              , cannonbullets = bullets'''
                              }
    where
        (_, cannon')       = checkCollisions (aliens world) [cannon world]        -- aliens touched cannon
        (rays, cannon'')   = checkCollisions (alienrays world) cannon'            -- rays touched cannon
        (bullets, aliens') = checkCollisions (cannonbullets world) (aliens world) -- bullets touched aliens
        (bullets', ufo')   = checkCollisions bullets [ufo world]                  -- bullets touched ufo
        (bullets'', rays') = checkCollisions bullets' rays                        -- bullets touched rays
        (bullets''', _)    = checkCollisions bullets'' (bunkers world)            -- bullets touched bunkers
        (rays'', _)        = checkCollisions rays' (bunkers world)                -- rays touched bunkers

-- clear all components flagged with destroy
updateDestroy :: [Picture] -> Float -> World a -> World a
updateDestroy sprites t world = world {
                              state = stt
                            , ufo = ufo'
                            , aliens = aliens''
                            , alienrays = clear (alienrays world)
                            , cannon = cannon'
                            , cannonbullets = clear (cannonbullets world)
                            , score = score world + alienscore + ufoscore
                            , lives = lives'
                            }
    where
        clear l = filter (not . destroy) l
        -- change sprite, change sprite id
        aliens' = parMap rpar (\x -> if destroy x && spriteid x /= -1 then x { sprite = sprites!!11, spriteid = -1, destroy = False } else x) (aliens world)
        aliens'' = clear aliens'
        alienscore
            | stt == Defeat = 0
            | otherwise     = sum $ parMap rpar (\x -> if destroy x then points x else 0) aliens'
        ufoscore
            | destroy $ ufo world = points $ ufo world
            | otherwise           = 0
        ufo'
            | destroy (ufo world) && points (ufo world) > 0        = (ufo world) { sprite = sprites!!12, spriteid = -1, destroy = False, clock = clock (ufo world) + t , points = 0}
            | spriteid (ufo world) < 0 && clock (ufo world) < 0.2  = (ufo world) { clock = clock (ufo world) + t }
            | spriteid (ufo world) < 0 && clock (ufo world) >= 0.2 = (ufo world) { sprite = Blank, spriteid = 6, clock = 0}
            | otherwise                                            = ufo world
        lives'
            | destroy $ cannon world = lives world - 1
            | otherwise              = lives world
        cannon'
            | destroy (cannon world) && lives' == 0 = cannon world
            | otherwise                             = (cannon world) { destroy = False }
        stt
            | lives' <= 0   = Defeat
            | null aliens'' = Victory
            | otherwise     = Playing