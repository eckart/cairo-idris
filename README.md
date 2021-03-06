Cairo Bindings for Idris
=======================

This project contains bindings for the cairo drawing library http://cairographics.org/


How to configure
----------------

You need to install libcairo separately on your machine.
This can be done in various platform dependent ways, so I won't bother
to write them down here as you can google them easily.

One of the methods described on this page may also work for you http://cairographics.org/download/

Additionally this project uses the idris SDL2 bindings to render to a window.

Troubleshooting
---------------
cairo uses pkg-config to determine the include and libs directories for compiling.
On Mac with brew this might fail with the message
```
Package xcb-shm was not found in the pkg-config search path.
Perhaps you should add the directory containing `xcb-shm.pc'
to the PKG_CONFIG_PATH environment variable
Package 'xcb-shm', required by 'cairo', not found
```

If that is the case you need to explicitly include the directory /opt/X11/lib/pkgconfig in
your PKG_CONFIG_PATH, like so:

export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig/:$PKG_CONFIG_PATH

Example
-------

This Package contains a low level monadic binding for some functions of libcairo as well as a simple effect for drawing.
There is even an example for the effectful usage for drawing a penrose tiling like this:

![Example of a penrose tiling](https://raw.githubusercontent.com/eckart/cairo-idris/master/examples/penrose/penrose.png)



