module Graphics.Cairo.Cairo

import Data.Fin
import Data.Vect
import Graphics.Color
import Graphics.SDL2.SDL
import Graphics.Cairo.Config


%include C "cairo_idris.h"
%include C "cairo.h"
%link C "cairo_idris.o"
%lib C "cairo"

%access private

||| The Cairo Surface
export
data Surface = Srf Ptr

||| A cairo drawing context
export
data DrawingCtx = Ctx Ptr

||| Glfw Window
export
data Window = Win Ptr

||| Glfw Window
export
data Pixels = MkPixels Ptr

public export
Point : Type
Point = (Double, Double)

-- -----------------------------------------------------------------------------
-- Lifecycle


||| Creates a cairo image surface
||| @ width the width of the image
||| @ height the height of the image
export
createSurface : (width: Int) -> (height: Int) -> IO Surface
createSurface width height = 
  do ptr <- foreign FFI_C "idr_cairo_create_image_surface" (Int -> Int -> IO Ptr) width height
     pure $ Srf ptr

||| creates a drawing context
||| @ surface the surface for the drawing context
export
createContext : (surface: Surface) -> IO DrawingCtx
createContext (Srf ptrSurface) =   
  do ptr <- foreign FFI_C "cairo_create" (Ptr -> IO Ptr) ptrSurface
     pure $ Ctx ptr
     
||| destroy the drawing context
export 
destroyContext : DrawingCtx -> IO ()
destroyContext (Ctx ptr) = foreign FFI_C "cairo_destroy" (Ptr -> IO ()) ptr
     

||| destroys the surface
export 
destroySurface : (surface: Surface) -> IO ()
destroySurface (Srf ptr) = 
  foreign FFI_C "cairo_surface_destroy" (Ptr -> IO ()) ptr
  
export
flushSurface : (surface: Surface) -> IO ()
flushSurface (Srf ptr) = foreign FFI_C "cairo_surface_flush" (Ptr -> IO ()) ptr 


export
createSurfaceData : Int -> Int -> IO Pixels 
createSurfaceData width height = do p <- foreign FFI_C "idr_create_surface_buffer" (Int -> Int -> IO Ptr) width height
                                    pure $ MkPixels p

                                  
export
createSurfaceForData : TextureRaw -> Int -> Int -> IO Surface
createSurfaceForData (MkTextureRaw ptr pitch) width height =
  do p <- foreign FFI_C "idr_surface_for_data" (Ptr -> Int -> Int -> Int -> IO Ptr) ptr width height pitch
     pure $ Srf p

public export                                    
freeSurfaceData : Pixels -> IO ()
freeSurfaceData (MkPixels ptr) = foreign FFI_C "free" (Ptr -> IO ()) ptr
                                  
-- -----------------------------------------------------------------------------
-- Fonts / Text (see http://cairographics.org/manual/cairo-text.html )

--cairo_select_font_face (cr, "serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD);

||| sets the font size for the current font
setFontSize : (ctx: DrawingCtx) -> (size: Double) -> IO ()
setFontSize (Ctx ptr) s = foreign FFI_C "cairo_set_font_size" (Ptr -> Double -> IO ()) ptr s 

||| Displays the text
showText : (ctx: DrawingCtx) -> (t: String) -> IO ()
showText (Ctx ptr) t = foreign FFI_C "cairo_show_text" (Ptr -> String -> IO ()) ptr t

--void 	cairo_glyph_path ()
--void 	cairo_text_path ()


-- -----------------------------------------------------------------------------
-- Drawing Context http://cairographics.org/manual/cairo-cairo-t.html

||| set the color
||| @ ctx the current drawing context
||| @ r the red component. 0-1 
||| @ g the green component. 0-1 
||| @ b the blue component. 0-1 
||| @ alpha the alpha 
public export 
setSourceRGBA : (ctx: DrawingCtx) -> (r: Double) -> (g: Double) -> (b: Double) -> (alpha: Double) -> IO ()
setSourceRGBA (Ctx ptr) r g b a = foreign FFI_C "cairo_set_source_rgba" (Ptr -> Double -> Double -> Double -> Double -> IO ()) ptr r g b a 


||| sets the line width for the current drawing context

||| @ ctx the current drawing context
||| @ width the line width
public export
setLineWidth : (ctx: DrawingCtx) -> (width: Double) -> IO ()
setLineWidth (Ctx ptr) width = foreign FFI_C "cairo_set_line_width" (Ptr -> Double -> IO ()) ptr width


||| A drawing operator that strokes the current path according to the current line width, line join, line cap, and dash settings
public export
stroke : (ctx: DrawingCtx) -> IO ()
stroke (Ctx ptr) = foreign FFI_C "cairo_stroke" (Ptr -> IO ()) ptr

||| A drawing operator that fills the current path according to the current fill rule, (each sub-path is implicitly closed before being filled)
public export
fill : (ctx: DrawingCtx) -> IO ()
fill (Ctx ptr) = foreign FFI_C "cairo_fill" (Ptr -> IO ()) ptr

-- -----------------------------------------------------------------------------
-- Paths, see also http://cairographics.org/manual/cairo-Paths.html

||| Begin a new sub-path. After this call the current point will be (x , y ).
public export 
moveTo : (ctx: DrawingCtx) -> (Double, Double) -> IO ()
moveTo (Ctx ptr) (x, y) = foreign FFI_C "cairo_move_to" (Ptr -> Double -> Double -> IO ()) ptr x y 


||| Adds a line to the path from the current point to position (x , y ) in user-space coordinates. After this call the current point will be (x , y ).
||| If there is no current point before the call to cairo_line_to() this function will behave as cairo_move_to(cr , x , y ).
public export 
lineTo : (ctx: DrawingCtx) -> (Double, Double) -> IO ()
lineTo (Ctx ptr) (x, y) = foreign FFI_C "cairo_line_to" (Ptr -> Double -> Double -> IO ()) ptr x y 

||| Adds a circular arc of the given radius to the current path. 
||| The arc is centered at (xc , yc ), begins at angle1 and proceeds in the direction of increasing angles to end at angle2 . 
||| If angle2 is less than angle1 it will be progressively increased by 2*M_PI until it is greater than angle1 .
||| Angles are measured in radians.
||| @ ctx the current drawing context
||| @ c x coordinate of the center of the arc 
||| @ r radius
||| @ angle1 the start angle, in radians
||| @ angle2 the end angle, in radians
public export 
arc : (ctx: DrawingCtx) -> (c: (Double, Double)) -> (r: Double) -> (angle1: Double) -> (angle2: Double) -> IO ()
arc (Ctx ptr) (cx, cy) r angle1 angle2 = 
  foreign FFI_C "cairo_arc" (Ptr -> Double -> Double -> Double -> Double -> Double -> IO ()) ptr cx cy r angle1 angle2

||| see documentation of arc.
public export 
arcNegative : (ctx: DrawingCtx) -> (Double, Double) -> (r: Double) -> (angle1: Double) -> (angle2: Double) -> IO ()
arcNegative (Ctx ptr) (cx, cy) r angle1 angle2 = 
  foreign FFI_C "cairo_arc_negative" (Ptr -> Double -> Double -> Double -> Double -> Double -> IO ()) ptr cx cy r angle1 angle2

--cairo_path_t * 	cairo_copy_path ()
--cairo_path_t * 	cairo_copy_path_flat ()
--void 	cairo_path_destroy ()
--void 	cairo_append_path ()
-- cairo_bool_t 	cairo_has_current_point ()
-- void 	cairo_get_current_point ()
-- void 	cairo_path_extents ()

||| Clears the current path. After this call there will be no path and no current point.
public export
newPath : (ctx: DrawingCtx) -> IO ()
newPath (Ctx ptr) = foreign FFI_C "cairo_new_path" (Ptr -> IO ()) ptr

||| Begin a new sub-path. Note that the existing path is not affected. After this call there will be no current point.
||| In many cases, this call is not needed since new sub-paths are frequently started with moveTo.
||| A call to newSubPath is particularly useful when beginning a new sub-path with one of the arc calls. 
||| This makes things easier as it is no longer necessary to manually compute the arc's initial coordinates for a call to cairo_move_to(
public export
newSubPath : (ctx: DrawingCtx) -> IO ()
newSubPath (Ctx ptr) = foreign FFI_C "cairo_new_sub_path" (Ptr -> IO ()) ptr

||| Adds a line segment to the path from the current point to the beginning of the current sub-path, 
||| (the most recent point passed to moveTo), and closes this sub-path. 
||| After this call the current point will be at the joined endpoint of the sub-path.
public export
closePath : (ctx: DrawingCtx) -> IO ()
closePath (Ctx ptr) = foreign FFI_C "cairo_close_path" (Ptr -> IO ()) ptr


||| Adds a cubic Bézier spline to the path from the current point to position (x3 , y3 ) in user-space coordinates, 
||| using (x1 , y1 ) and (x2 , y2 ) as the control points. After this call the current point will be (x3 , y3 ).
public export
curveTo : (ctx: DrawingCtx) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> IO ()
curveTo (Ctx ptr) (x1, y1) (x2, y2) (x3, y3) = 
  foreign FFI_C "cairo_curve_to" (Ptr -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()) ptr x1 y1 x2 y2 x3 y3

||| Adds a closed sub-path rectangle of the given size to the current path at position (x , y ) in user-space coordinates.
export
rectangle : (ctx: DrawingCtx) -> (p: Point) -> (width: Double) -> (height: Double) -> IO ()
rectangle (Ctx ptr) (x,y) width height =
  foreign FFI_C "cairo_rectangle" (Ptr -> Double -> Double -> Double -> Double -> IO()) ptr x y width height

||| Relative-coordinate version of cairo_curve_to(). 
||| All offsets are relative to the current point. Adds a cubic Bézier spline to the path from the current point 
||| to a point offset from the current point by (dx3 , dy3 ), using points offset by (dx1 , dy1 ) 
||| and (dx2 , dy2 ) as the control points. After this call the current point will be offset by (dx3 , dy3 ).
public export
relCurveTo : (ctx: DrawingCtx) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> IO ()
relCurveTo (Ctx ptr) (dx1, dy1) (dx2, dy2) (dx3, dy3) = 
  foreign FFI_C "cairo_rel_curve_to" (Ptr -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()) ptr dx1 dy1 dx2 dy2 dx3 dy3

||| Relative-coordinate version of cairo_line_to(). 
||| Adds a line to the path from the current point to a point that is offset from the current point by (dx , dy ) 
||| in user space. After this call the current point will be offset by (dx , dy ).
public export 
relLineTo : (ctx: DrawingCtx) -> (Double, Double) -> IO ()
relLineTo (Ctx ptr) (dx, dy) = foreign FFI_C "cairo_rel_line_to" (Ptr -> Double -> Double -> IO ()) ptr dx dy 

||| Begin a new sub-path. After this call the current point will offset by (dx , dy ).
public export 
relMoveTo : (ctx: DrawingCtx) -> (Double, Double) -> IO ()
relMoveTo (Ctx ptr) (dx, dy) = foreign FFI_C "cairo_rel_move_to" (Ptr -> Double -> Double -> IO ()) ptr dx dy 

-- -----------------------------------------------------------------------------
-- Transformations

-- see http://cairographics.org/manual/cairo-Transformations.html


||| Modifies the current transformation matrix (CTM) by translating the user-space origin by (tx , ty ). 
||| This offset is interpreted as a user-space coordinate according to the CTM in place before the new call to cairo_translate(). 
||| In other words, the translation of the user-space origin takes place after any existing transformation.
public export
translate : (ctx : DrawingCtx) -> (tx: Double, ty: Double) -> IO ()
translate (Ctx ptr) tx ty = foreign FFI_C "cairo_translate" (Ptr -> Double -> Double -> IO ()) ptr tx ty 

||| Modifies the current transformation matrix (CTM) by scaling the X and Y user-space axes by sx and sy respectively. 
||| The scaling of the axes takes place after any existing transformation of user space.
public export
scale : (ctx : DrawingCtx) -> (sx: Double, sy: Double) -> IO ()
scale (Ctx ptr) sx sy = foreign FFI_C "cairo_scale" (Ptr -> Double -> Double -> IO ()) ptr sx sy 

||| Modifies the current transformation matrix (CTM) by rotating the user-space axes by angle radians. 
||| The rotation of the axes takes places after any existing transformation of user space. 
||| The rotation direction for positive angles is from the positive X axis toward the positive Y axis.             
public export
rotate : (ctx : DrawingCtx) -> (angle: Double) -> IO ()
rotate (Ctx ptr) angle = foreign FFI_C "cairo_rotate" (Ptr -> Double -> IO ()) ptr angle
             
-- -----------------------------------------------------------------------------
-- Import / Export

-- see http://cairographics.org/manual/cairo-PNG-Support.html

||| writes the surface to an png 
||| TODO this should only work on an image surface
||| @ surface the surface to export as a PNG
||| @ filename file name of the PNG
export
writePNG : (surface: Surface) -> (filename: String) -> IO ()
writePNG (Srf ptr) filename = 
  foreign FFI_C "cairo_surface_write_to_png" (Ptr -> String -> IO ()) ptr filename

||| creates a cairo image surface from a png
||| @ filename filename of the png
export 
createSurfaceFromPNG : (filename: String) -> IO Surface
createSurfaceFromPNG filename = do ptr <- foreign FFI_C "cairo_image_surface_create_from_png" (String -> IO Ptr) filename
                                   pure $ Srf ptr
                                   
                                   
                                   
public export 
draw : (ctx: DrawingCtx) -> Vect (S k) (Double, Double) -> IO ()
draw ctx (x :: xs) = do 
                        moveTo ctx x
                        draw' xs
                     where draw' : Vect n (Double, Double) -> IO ()
                           draw' []        = pure ()
                           draw' (y :: ys) = do lineTo ctx y
                                                draw' ys

                                   
