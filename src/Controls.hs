module Controls where

-- libraries
import Graphics.Gloss.Interface.IO.Interact
    ( Event(EventKey),
      Key(Char, MouseButton, SpecialKey),
      KeyState(Up, Down),
      MouseButton(LeftButton),
      SpecialKey(KeyRight, KeySpace, KeyLeft) )

import World ( World(shoot, goLeft, goRight) )

-- handle control events
controlEvent :: Event -> World a -> World a

-- confirm and fire keys
-- SPACE
controlEvent (EventKey (SpecialKey KeySpace) Down _ _) world    = world { shoot = True }
controlEvent (EventKey (SpecialKey KeySpace) Up _ _) world      = world { shoot = False }
-- LEFT CLICK
controlEvent (EventKey (MouseButton LeftButton) Down _ _) world = world { shoot = True }
controlEvent (EventKey (MouseButton LeftButton) Up _ _) world   = world { shoot = False }

-- go left keys
-- LEFT ARROW
controlEvent (EventKey (SpecialKey KeyLeft) Down _ _) world     = world { goLeft = True }
controlEvent (EventKey (SpecialKey KeyLeft) Up _ _) world       = world { goLeft = False }
-- A
controlEvent (EventKey (Char 'a') Down _ _) world               = world { goLeft = True }
controlEvent (EventKey (Char 'a') Up _ _) world                 = world { goLeft = False }

-- go right keys
-- RIGHT ARROW
controlEvent (EventKey (SpecialKey KeyRight) Down _ _) world    = world { goRight = True }
controlEvent (EventKey (SpecialKey KeyRight) Up _ _) world      = world { goRight = False }
-- D
controlEvent (EventKey (Char 'd')  Down _ _) world              = world { goRight = True }
controlEvent (EventKey (Char 'd')  Up _ _) world                = world { goRight = False }

-- otherwise do nothing
controlEvent _ world = world