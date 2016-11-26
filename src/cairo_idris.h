#ifndef __CAIRO_IDRIS_H
#define __CAIRO_IDRIS_H

#include <idris_rts.h>
#include <cairo.h>

void* idr_surface_for_data(void* buffer, int width, int height, int pitch);

void* idr_cairo_create_image_surface(int width, int height);

// --------------------------------------------------------------------------
// OpenGL / GLFW
// --------------------------------------------------------------------------

void* idr_glfw_create_window(int width, int height);

void* idr_create_surface_buffer(int width, int height);

#endif
