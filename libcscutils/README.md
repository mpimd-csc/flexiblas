# libcscutils 

libcscutils provides various utilities that are useful for the development of 
your own software. 

At the moment libcscutils contains the following modules: 
- IniFile: Reading and Writing of key-value based configuration files. 
- IO: Wrapper around the libc IO routines to include compression support (ZLIB, BZIP2, LZMA). 
- Threading: Support function for threaded applications like pthread locked queues. 
- Error Message: fprintf/printf replacements for flexible printing of error, warning and information messages 
- HDF5: wrapper around HDF5 routine for easy reading and writing of matrices and vectors. 
- DS: Datastructures 


## Documentation
All modules are documented using doxygen. In order to generate the whole 
documentation check out the repository and run 
 
```
  mkdir build && cd build
  cmake ../ 
  make doc 
  firefox doc/explore-html/index.html
```
