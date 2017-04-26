/**
@mainpage
libcscutils is a small software library/code collection containing useful building blocks for the 
development of you own software. All submodules can be copied out of the source tree and used independently
from the rest of the software package. 

At the moment the library includes the following utilities: 
\li \ref inifile 
\li \ref io 
\li \ref hdf5
\li \ref threading 
\li \ref image 
\li \ref error_message
\li \ref hardware
\li \ref ds
\li \ref strutils
\li \ref table 

@section integrate Integration in own software packages

The easiest way to include libcscutils in your software is to add a git submodule into your code repository. This is done 
using 
\code
git submodule add git@gitlab.mpi-magdeburg.mpg.de:software/libcscutils.git
\endcode 
Afterwards, add <tt>licscutils/include</tt> to you include path and add the necessary source files from <tt>libcscutils/src</tt> to your \c Makefile. 

If you are using a CMake based build system you only need to add 
\code
add_subdirectory(libcscutils) 
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/libcscutils/include}
\endcode
to your master \c CMakeLists.txt and link cscutils to all targets where you need it. 
*/ 


/**
 * @defgroup threading Threading: Helper functions for multi-threaded application. 
 * 
 * */
