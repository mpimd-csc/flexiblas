= Configuration options for libcscutils


== Module Selection == 
Using the *CSCUTILS_FEATURES* variable one can set the functionality of the
libcscutils. Therefore the following features can be enabled using a semicolon
separated list: 
* inifile - Enable the inifile parser 
* io - Enable the IO subsystem 
* threading - Enable threading helpers 
* hdf5 - Enable HDF5 helpers 
* image - Enable image helpers
* hardware - Enable hardware related functions
* ds - Enable data structure related functions. 

== I/O Routines == 
The following options are boolean options and can be set to *ON* or *OFF*:
* CSC_IO_ZLIB - Enable/Disable zlib support.
* CSC_IO_BZIP2 - Enable/Disable bzip2 support.
* CSC_IO_LIBLZMA - Enable/Disable lzma support. 
* CSC_IO_MMAP - Enable/Disable mmap support. 



