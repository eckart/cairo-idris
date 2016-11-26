Cairo Bindings for Idris
=======================

This project contains bindngs for the cairo drawing library http://cairographics.org/


How to configure
----------------

You need to install libcairo separately on your machine.
This can be done in various platform dependent ways, so I won't bother
to write them down here as you can google them easily.

One of the methods described on this page may also work for you http://cairographics.org/download/

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




