#include <stdio.h>
#include <cairo.h>
#include <idris_rts.h>

void* idr_cairo_create_image_surface(int width, int height) {
  cairo_surface_t *surface =
            cairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
  return surface;
}

// --------------------------------------------------------------------------
// OpenGL / GLUT
// --------------------------------------------------------------------------

void* idr_create_surface_buffer(int width, int height) {
    int stride;

    stride = cairo_format_stride_for_width (CAIRO_FORMAT_ARGB32, width);
    return malloc (stride * height);
}

void* idr_surface_for_data(void* buffer, int width, int height, int pitch) {
    return cairo_image_surface_create_for_data (
					       (unsigned char *) buffer,
					       CAIRO_FORMAT_ARGB32,
					       width,
					       height,
					       pitch);
}

