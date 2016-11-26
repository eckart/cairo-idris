module Main

import Data.Fin
import Effects
import Effect.StdIO
import Graphics.Color
import Graphics.Shape
import Graphics.SDL2.SDL
import Graphics.SDL2.Effect
import Graphics.Cairo.Cairo
import Graphics.Cairo.Effect


Prog : (sdl: Type) -> (cairo: Type) -> (stdio : Type) -> Type
Prog sdl cairo t = { [SDL sdl,      -- the SDL effect
                      CAIRO cairo,  -- Cairo effect
                      STDIO]        -- a std io effect 
                     } Eff t

-- Convenient shorthand for initialised SDL
Running : Type -> Type
Running t = Prog SDLCtx () t

draw : Running ()
draw = with Effects do
  renderClear white
  ctx <- getSdlCtx
  getCanvas ctx
  setSourceRGBA (RGBA 128 250 128 255)
  center
  --line (0,0) (100, 100)
  triangle (-100,0) (100, 0) (0,100)
  --polygon $ (Line (-100, 0) (100, 0)) `Append` (0, 100)
  fill
  closeCanvas 
  render

process : Maybe Event -> Bool
process (Just (KeyDown KeyEsc)) = False
process (Just (AppQuit))        = False
process _                       = True

emain : Prog () () ()
emain = do putStrLn "Initialising"
           putStrLn "..."
           initialise "SDL2 Test" 800 600
           putStrLn "Initialised"
           eventLoop
           quit
           pure ()
        where 
          eventLoop : Running ()
          eventLoop = do draw
                         when (process (!poll)) eventLoop
main : IO ()
main = runInit [(), -- initial state for the SDL2 effect (nothing needs to go in here)
                (),
                ()  -- initial state for the StdIO effect 
                ] 
       emain



