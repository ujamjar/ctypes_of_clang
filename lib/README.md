Run `make cstdlib` to generate a ctypes version of the C
standard library in this directory called `Cstdlib`.

The (rather large) file will contain a module for each of
the standard header files.  Each module defines the related
types and a functor containing the related functions and 
variables.  The functor can be instantiated in a `libffi`
of `Cstubs` version.
