module Controls where

import Graphics.Gloss.Interface.IO.Interact

import World

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