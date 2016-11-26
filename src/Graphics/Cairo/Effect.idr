module Graphics.Cairo.Effect

import Data.Vect
import Effects
import Graphics.SDL2.Effect as SDL
import Graphics.Color
import Graphics.Shape
import Graphics.Cairo.Cairo as C


%access public export 
%default total

data CairoCtx = MkCairoCtx (Int, Int) SDLRenderer SDLTexture Surface DrawingCtx

data OpenPathCtx = MkOpenPath CairoCtx

drawingCtx : CairoCtx -> DrawingCtx
drawingCtx (MkCairoCtx _ _ _ _ ctx) = ctx

windowSize : CairoCtx -> (Int, Int)
windowSize (MkCairoCtx s _ _ _ ctx) = s

-- Effect resultValue inputResource outputResource
data Cairo : Effect where
     ||| creates the window + renderer. the effect has no result value, consumes no resource and
     ||| the output resource will have type SDLRenderer
     GetCanvas   : SDLCtx               -> sig Cairo () ()          CairoCtx
     CloseCanvas :                         sig Cairo () CairoCtx    ()
     OpenPath    :                         sig Cairo () CairoCtx    OpenPathCtx
     AddPath     :                         sig Cairo () OpenPathCtx OpenPathCtx
     ClosePath   :                         sig Cairo () OpenPathCtx CairoCtx
     Center      :                         sig Cairo () CairoCtx    CairoCtx
     WithCairo   : (DrawingCtx -> IO a) -> sig Cairo a  CairoCtx  CairoCtx

implementation Handler Cairo IO where
  handle ()  (GetCanvas (win, renderer)) k = 
         do (width, height) <- getWindowSize win 
            texture <- sdlCreateTexture renderer SDL_PIXELFORMAT_ARGB8888 SDL_TEXTUREACCESS_STREAMING width height
            tex <- lockTexture texture
            srf <- C.createSurfaceForData tex width height
            ctx <- C.createContext srf
            k () (MkCairoCtx (width, height) renderer texture srf ctx)
  handle (MkCairoCtx _ renderer texture srf ctx) CloseCanvas k = 
         do C.flushSurface srf
            C.destroyContext ctx
            C.destroySurface srf

            unlockTexture texture
            renderCopyFull renderer texture
            k () ()
  handle s (WithCairo f) k = do res <- f (drawingCtx s)
                                k res s
  handle s Center k = do let (w,h) = windowSize s
                         translate (drawingCtx s) (cast w / 2) (cast h / 2)
                         k () s 

CAIRO : Type -> EFFECT
CAIRO res = MkEff res Cairo

CAIRO_OFF : EFFECT
CAIRO_OFF = CAIRO ()

CAIRO_ON : EFFECT
CAIRO_ON = CAIRO CairoCtx

-- ------------------------------------------------------------------------
-- API
-- ------------------------------------------------------------------------

getCanvas : SDLCtx -> { [CAIRO_OFF] ==> [CAIRO_ON] } Eff () 
getCanvas sdlCtx = call $ GetCanvas sdlCtx

closeCanvas : { [CAIRO_ON] ==> [CAIRO_OFF] } Eff ()
closeCanvas = call $ CloseCanvas 

-- Drawing Context

setSourceRGBA : Color -> { [CAIRO_ON] } Eff () 
setSourceRGBA color = let (r, g, b, a) = colorToDoubles color
                      in call $ WithCairo (\ctx => C.setSourceRGBA ctx r g b a)

setLineWidth : Double -> { [CAIRO_ON] } Eff () 
setLineWidth width = call $ WithCairo (\ctx => C.setLineWidth ctx width)

stroke : { [CAIRO_ON] } Eff () 
stroke = call $ WithCairo (\ctx => C.stroke ctx)

fill : { [CAIRO_ON] } Eff () 
fill = call $ WithCairo (\ctx => C.fill ctx)

-- Shapes

line : (Double, Double) -> (Double, Double) -> { [CAIRO_ON] } Eff () 
line from to 
    = call $ WithCairo (\ctx => do 
           C.moveTo ctx from
           C.lineTo ctx to
           C.stroke ctx)

polygon : Path (S (S k)) -> { [CAIRO_ON] } Eff () 
polygon p = call $ WithCairo (\ctx => do C.newPath ctx
                                         C.draw ctx (toPoints p)) 

triangle : (Double, Double) -> (Double, Double) -> (Double, Double) -> { [CAIRO_ON] } Eff () 
triangle a b c = polygon $ (Line a b) `Append` c
-- transformations

center : { [CAIRO_ON] } Eff () 
center = call Center

rotate : (angle: Double) -> { [CAIRO_ON] } Eff () 
rotate angle = call $ WithCairo (\ctx => C.rotate ctx angle)
