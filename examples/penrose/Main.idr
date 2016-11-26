module Main

import Data.Fin
import Effects
import Effect.StdIO
import Data.Complex
import Graphics.Color
import Graphics.SDL2.SDL
import Graphics.SDL2.Effect
import Graphics.Cairo.Effect
import Graphics.Shape

%default total

-- ----------------------------------------------------------------
-- a bit of infrastructure
-- ----------------------------------------------------------------

C : Type
C = Complex Double

||| Division of Complex Numbers
divC : C -> C -> C
divC (a:+b) (c:+d) = let real = (a*c+b*d) / (c*c+d*d)
                         imag = (b*c-a*d) / (c*c+d*d)
                     in
                        (real:+imag)

goldenRatio : Double
goldenRatio = 0.5 * (1 + (sqrt 5))

Tri : Type
Tri = (C, C, C)

-- ----------------------------------------------------------------
-- Calculate the penrose tiling
-- ----------------------------------------------------------------

data Penrose = Red Tri | Blue Tri

implementation Show Penrose where
    show (Red x)  = "Red " ++ show x
    show (Blue x) = "Blue " ++ show x

||| Penrose subdivide
||| This computes a single step in the tiling
||| see also: http://preshing.com/20110831/penrose-tiling-explained/
penroseSubdivide : Penrose -> List Penrose
penroseSubdivide (Blue (a, b, c)) = let q = b + ((a-b) `divC` (goldenRatio :+ 0))
                                        r = b + ((c-b) `divC` (goldenRatio :+ 0))
                                    in [Blue (r, c, a), Blue (q, r, b), Red (r, q, a)]
penroseSubdivide (Red (a, b, c))  = let p = a + ((b-a) `divC` (goldenRatio :+ 0))
                                    in [Red (c, p, b), Blue (p, c, a)]

||| A Stream of all Tilings for a given initial triangle
||| a single tiliing consists of a list of triangles
penroseTiling : Tri -> Stream (List Penrose)
penroseTiling triangle = penroseTiling' [(Red triangle)]
                         where penroseTiling' : (List Penrose) -> Stream (List Penrose)       
                               penroseTiling' triangles = iterate (\tris => tris >>= penroseSubdivide) triangles


||| Creates an initial upside down triangle 
||| @angle the angle in degree
initialTriangle : Int -> (angle: Double) -> Tri
initialTriangle height angle = let rad = 2 * pi * angle / 360
                                   y = (tan (rad / 2)) * (cast height)
                               in ( 0:+0,  (-1) * y :+ (-1) * (cast height), y :+ (-1) * (cast height))

penroseTriangles : Nat -> Int -> List Penrose
penroseTriangles level numTriangles = let angle = 360.0 / (cast numTriangles)
                                          tilings = penroseTiling $ initialTriangle 200 angle
                                      in head (drop level tilings)

   
-- ----------------------------------------------------------------
-- Cairo Rendering
-- ----------------------------------------------------------------

red : Color
red = RGBA 250 140 89 255

green : Color
green = RGBA 143 206 94 255

getColor : Penrose -> Color
getColor (Red  _) = red
getColor (Blue _) = green

getTriangle : Penrose -> Tri
getTriangle (Red triangle) = triangle 
getTriangle (Blue triangle) = triangle 
 
renderTriangle : Penrose -> { [CAIRO_ON] } Eff () 
renderTriangle penrose = with Effects do
  setSourceRGBA $ getColor penrose
  setLineWidth 6
  let (x1:+y1, x2:+y2, x3:+y3) = getTriangle penrose
  triangle (x1, y1) (x2, y2) (x3, y3)
  fill

renderTriangles : List Penrose -> Double -> Nat -> { [CAIRO_ON] } Eff () 
renderTriangles triangles angle Z = pure ()
renderTriangles triangles angle (S k) = with Effects do renderTriangle' triangles
                                                        rotate angle
                                                        renderTriangles triangles angle k
                                                     where 
    renderTriangle' : List Penrose -> { [CAIRO_ON] } Eff () 
    renderTriangle' [] = pure ()
    renderTriangle' (t :: ts) = with Effects do
          renderTriangle t
          renderTriangle' ts

-- ----------------------------------------------------------------
-- Setup and Main Loop
-- ----------------------------------------------------------------
    
Prog : (sdl: Type) -> (cairo: Type) -> (result : Type) -> Type
Prog sdl cairo t = { [SDL sdl,      -- the SDL effect
                      CAIRO cairo,  -- Cairo effect
                      STDIO]        -- a std io effect 
                     } Eff t

Running : Type -> Type
Running t = Prog SDLCtx () t

        
data AppState = MkState Nat Nat

draw : AppState -> Running ()
draw (MkState angles level) =  with Effects do
  renderClear white
  ctx <- getSdlCtx
  getCanvas ctx
  center

  let numTriangles = cast angles + 4
  let solution = penroseTriangles level numTriangles
  let angle = (2 * pi / (cast numTriangles))
    
  renderTriangles solution angle (cast numTriangles)                                                                       

  closeCanvas 
  render


process : AppState -> Maybe Event -> Maybe AppState
process _                       (Just (KeyDown KeyEsc))        = Nothing
process _                       (Just (AppQuit))               = Nothing
process (MkState angles level)  (Just (KeyDown KeyUpArrow))    = Just (MkState (S angles) level)
process (MkState Z level)       (Just (KeyDown KeyDownArrow))  = Just (MkState Z level)
process (MkState (S k) level)   (Just (KeyDown KeyDownArrow))  = Just (MkState k level)
process (MkState angles level)  (Just (KeyDown KeyRightArrow)) = Just (MkState angles (S level))
process (MkState angles Z)      (Just (KeyDown KeyLeftArrow))  = Just (MkState angles Z)
process (MkState angles (S k))  (Just (KeyDown KeyLeftArrow))  = Just (MkState angles k)
process state                   _                              = Just state


%default partial

emain : Prog () () ()
emain = do putStrLn "Initialising"
           initialise "Penrose" 800 600
           putStrLn "Initialised"
           eventLoop (MkState Z Z)
           quit
           pure ()
        where 
          eventLoop : AppState -> Running ()
          eventLoop state = do draw state 
                               ev <- poll
                               let res = process state ev
                               case res of
                                 (Just newState) => eventLoop newState
                                 _               => pure ()

main : IO ()
main = runInit [(), -- initial state for the SDL2 effect (nothing needs to go in here)
                (),
                ()  -- initial state for the StdIO effect 
                ] 
       emain



