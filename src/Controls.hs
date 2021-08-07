module Controls where

import Graphics.Gloss.Interface.IO.Interact

import World

-- handle input events
controlEvent :: Event -> World a -> World a
-- space to confirm and fire
controlEvent (EventKey (SpecialKey KeySpace) Down _ _) world = world { shoot = True }
controlEvent (EventKey (SpecialKey KeySpace) Up _ _) world   = world { shoot = False }
-- click to confirm and fire
controlEvent (EventKey (MouseButton LeftButton) Down _ _) world = world { shoot = True }
controlEvent (EventKey (MouseButton LeftButton) Up _ _) world   = world { shoot = False }
-- left arrow to go left
controlEvent (EventKey (SpecialKey KeyLeft) Down _ _) world  = world { goLeft = True }
controlEvent (EventKey (SpecialKey KeyLeft) Up _ _) world    = world { goLeft = False }
-- right arrow to go right
controlEvent (EventKey (SpecialKey KeyRight) Down _ _) world = world { goRight = True }
controlEvent (EventKey (SpecialKey KeyRight) Up _ _) world   = world { goRight = False }
-- letter a to go left
controlEvent (EventKey (Char 'a') Down _ _) world            = world { goLeft = True }
controlEvent (EventKey (Char 'a') Up _ _) world              = world { goLeft = False }
-- letter d to go right
controlEvent (EventKey (Char 'd')  Down _ _) world           = world { goRight = True }
controlEvent (EventKey (Char 'd')  Up _ _) world             = world { goRight = False }
controlEvent _ world = world