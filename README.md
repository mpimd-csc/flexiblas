# FlexiBLAS - A runtime BLAS switch (mpi-magdeburg.mpg.de/projects/flexiblas)
**Version 2.0.0**

Copyright 2013--2017 by Martin Köhler (MPI Magdeburg) 
                        Jens Saak (MPI Magdeburg)

**FlexiBLAS** is a wrapper library that enables the exchange of the *BLAS* and
*LAPACK* implementation used by a program without recompiling or relinking it.
This could in principle also be achieved using the `LD_LIBRARY_PATH`
mechanism, if all *BLAS* and *LAPACK* implementations consistently implement a
single library file that implements a fully compatible interface. Unfortunately,
not all *BLAS* and *LAPACK* implementations consists of only this exact one
shared library containing all required symbols. The requirement for different
numbers of library (shared object) files clearly breaks the possibility to
easily switch via the `LD_LIBRARY_PATH` mechanism.

The **FlexiBLAS** library provides a *GNU Fortran* compatible interface to all
functions and subroutines provided by the *Netlib* reference implementations. As
backends **FlexiBLAS** can employ all *BLAS* and *LAPACK* implementations which
consist of one shared library directly. Other variants like the *Intel MKL* or
*ATLAS* that use multiple files are integrated using a single surrogate library
wrapping the different files into one. 

## Setup

The build system used by **FlexiBLAS** is *cmake* which prefers an out-of-source 
build:
```
    mkdir build 
    cd build
    cmake ../ <FURTHEROPTIONS>
    make 
    make install
```
If the build system should try to build backends for *BLAS* and *LAPACK*
libraries that are not known to the *FindBLAS cmake* module, or reside in
non-standard paths on your system, make sure to include their location
(containing directory) to the `LD_LIBRARY_PATH` environment variable. At the
end of the configuration step, *cmake* will inform you about the additional
backends found on your system.

The configuration via *cmake* can be influenced further by the following
options:

* `-DCMAKE_INSTALL_PREFIX=/PATH/`  
  Defines an alternative location for the installation of the resulting 
  library. The default installation location is `/usr/local`. 
  The library called `libflexiblas.so` is installed in 
  `CMAKE_INSTALL_PREFIX/lib` and the default search path for backends is 
  `CMAKE_INSTALL_PREFIX/lib/flexiblas`.

* `-DDEBUG=ON/OFF`  
  Turns the inclusion of debugging symbols `ON` or `OFF`. The default is
  `OFF`.

* `-DINTEGER8=ON/OFF`  
  Enables 8-byte integers for the *Fortran* interface. Be aware that the 
  application and the backend libraries must be compiled to support 8-byte
  integers as well. Since most applications and libraries still us the 
  classic 32-bit integers the default is `OFF`.

* `-DCBLAS=ON/OFF`  
  Enables the *CBLAS* frontend. If the backend itself contains a *CBLAS*
  interface that one is used. Otherwise **FlexiBLAS** acts like the
  *CBLAS*-wrapper from *Netlib*. This option is set to `ON` by default.

* `-DABI=GNU|Intel`  
  Selects the ABI of the Fortran Interface. Per default the compiler used
  determines which ABI is used, but it can also be forced using this
  parameter. The difference between both ABIs is how complex values are
  returned from Functions. The *GNU*-style returns them as a normal return value
  on the stack. The *Intel* (*F2C*, *GNU F77*) style returns them as an
  additional parameters in front of the proper function arguments. That means it
  is a subroutine from the *C* point of view.

* `-DBLASNAME=LIBS`  
  Override the automatic search for a *BLAS* backend. The *BLASNAME* is the name
  of the backend in upper case. The libs argument is a semicolon separated list
  of libraries which are necessary to link against this *BLAS* library. For
  example if we want to compile the *OpenBLAS* backend out of a static
  *OpenBLAS* library, we use:
  `-DOPENBLAS="/path/to/libopenblas.a;gfortran;gomp;m"`. Using this
  configuration variable the detection of a special *BLAS* library can be turned
  off via setting it to `OFF`.

* `-DMKL_CUSTOM=ON/OFF`  
  Turn `ON`/`OFF` the build of an *Intel MKL* custom library instead of
  searching for the *MKL* directly. This is necessary if you plan to switch
  between single and multithreaded *MKL* in your application. Turning this
  option on reuqires to have the `MKLROOT` environment variable set properly
  and an *MKL* version >=11.1. The default is `OFF`.

* `-DDEV=ON/OFF`  
  Configures the build to work only in the build directory. In this 
  case **FlexiBLAS** searches in the root of the build directory for its 
  configuration file and the build directory is added to the default library 
  search path. Turning on this option is only useful during the development in 
  order to work independently from an installed **FlexiBLAS**. If you plan to
  install **FlexiBLAS**, set it to `OFF` which is also the default value.

* `-DLAPACK=ON/OFF`  
  Turn `ON/OFF` the *LAPACK* support of **FlexiBLAS**. If enabled
  **FlexiBLAS** also includes the wrappers around *LAPACK*. Default value is
  `ON`. 

The `PROFILE=ON/OFF` option was removed from version 1.1.0 onwards; profiling
is now integrated in the standard configuration. It is enabled at runtime using
the `FLEXIBLAS_PROFILE` environment variable.

The following *BLAS* backends are tested during development:
* *Netlib* Reference Implementation 
* *ATLAS* 
* *OpenBLAS*
* *Intel MKL*
* *AMD ACML*

## Setup on *Debian* and Derivates 
The easiest way is to build a *Debian* package out of the source and install it. 
To this end you have to install a small set of prerequisites before using:
```
    sudo apt-get update
    sudo apt-get install debhelper cmake gfortran gcc libatlas-base-dev
    sudo apt-get install libopenblas-dev build-essential fakeroot
```
Afterwards you can build the *Debian* package out of the source code by typing:
```
    fakeroot dpkg-buildpackage -us -uc 
```
inside the extracted source. After building the package the source directory can 
be cleaned using
```
    debian/rules clean 
``` 
The build process creates a set of packages which are available one directory 
level above. They are installed using:
```
    sudo dpkg -i ../libflexiblas-*.deb 
```    
If you already have a version of **FlexiBLAS** installed you have to remove it
and its configuration files beforehand by: 
```
    sudo apt-get remove --purge libflexiblas libflexiblas-dev
    sudo rm /etc/flexiblasrc
    rm ~/.flexiblasrc
```

### Caveats
* If **FlexiBLAS** is not automatically selected by the `update-alternatives`
  mechanism you have to select it via:
```
    sudo update-alternatives --config libblas.so
    sudo update-alternatives --config libblas.so.3
    sudo update-alternatives --config libblas.so.3gf
```
  The last one is only necessary up to *Ubuntu 12.04* and *Debian 6*.

* **FlexiBLAS** requires that you use the reference implementation of *LAPACK*
  that means if update-alternatives uses the *LAPACK* implementation provided by
  *ATLAS* many programs will fail. Please install the reference *LAPACK* package
  and set it in the `update-alternatives` mechanism via
```
    sudo update-alternatives --config liblapack.so
    sudo update-alternatives --config liblapack.so.3
    sudo update-alternatives --config liblapack.so.3gf
```
  The last one is only necessary upto *Ubuntu 12.04* and *Debian 6*.

* Due to a bug in the *Ubuntu* *OpenBLAS* packages **FlexiBLAS** does not work
  with the **OpenBLAS** package versions between 0.2.6 and 0.2.8-4. If you have
  one of these version installed, please update *OpenBLAS* before installing
  **FlexiBLAS**. 

## Setup on *Gentoo* 
Make sure you have the *science overlay* installed (see:
http://wiki.gentoo.org/wiki/Project:Science/Overlay for further information) 
to get the latest versions of *BLAS* and *LAPACK* packages. You can either
compile the library directly from the source tar-balls, or use our local
*portage overlay*. For the latter download the `portage.tgz` above to your home
directory. Then do:
```
    cd /usr/local/
    tar -xvzf ~/portage.tgz 
```
Now add the following to your systems `make.conf` (usually located in either 
`/etc/` or `/etc/portage/`)
```
    # PORTDIR_OVERLAY specifies the location of a local Portage overlay
    PORTDIR_OVERLAY="${PORTDIR_OVERLAY} /usr/local/portage/"
```
to register the local overlay. Finally you need to soft unmask the latest *BLAS*
and *LAPACK* versions from the science overlay and our **Flexiblas** versions in
your `package.keywords` or `package.accept_keywords` files:
```
    <sci-libs/flexiblas-99999999 ~x86
    sci-libs/blas-reference  ~x86
    sci-libs/cblas-reference ~x86
    sci-libs/lapack-reference ~x86
    virtual/blas ~x86
    virtual/lapack ~x86
    virtual/cblas ~x86
    sci-libs/gsl ~x86 
    sci-libs/eselect-cblas ~x86 
    sci-libs/atlas ~x86
    sci-libs/gotoblas2 ~x86
```
Now to install the actual library simply do:
```
    emerge -av flexiblas
```
In case you have you have *Intels MKL* installed, you can activate support by
adding:
```
    sci-libs/flexiblas mkl
```
to your `/etc/portage/package.use` file. After successful installation use
`eselect blas list`, `eselect cblas list`, and the coresponding set commands
to choose **FlexiBLAS** as your standard *BLAS* implementation for both
*Fortran* and *C* access. You may also want to do `equery depends cblas blas`
to find the packages depending on *BLAS* and recompile/reinstall them using
`emerge -1` packagename.

## Configuration Files 
**FlexiBLAS** reads a set of configuration files at startup. These are:
```
    CMAKE_INSTALL_PREFIX/etc/flexiblasrc 
```
and:
```
    ~/.flexiblasrc
```
or:
```
    CMAKE_INSTALL_PREFIX/etc/flexiblasrc64 
```
and:
```
    ~/.flexiblasrc64
```
in case of a 64-bit integer build. 

The files contain a mapping between the name of a backend and the corresponding
name of the shared object. The setting in the user owned file overrides the 
global settings. All settings are superseded eventually by setting environment
variables. The syntax of the file is similar to the *KDE* configuration files or 
the *ini*-files known from early *MS-Windows* versions. Basically it is a
key-value store with section support. In the first section of the configuration
file are global definition like the default *BLAS* backend or the verbosity
level: 
```
    default = NETLIB
    verbose = 0 
```
Than for each *BLAS* backend there is a block of the following style: 
```
    [NAME_OF_THE_BACKEND]
    library = shared_object.so
    comment = Information text about the BLAS backend
```
If the library argument defines a relative path, **FlexiBLAS** searches inside
its default path (`CMAKE_INSTALL_PREFIX/lib/flexiblas` or 
`CMAKE_INSTALL_PREFIX/lib/flexiblas64`) for this shared object. If the path is
absolute it tries to open the library directly. The name of the backend is
case-insensitive!

Additionally, one can define additional search paths in the first section.
Therefore one has to add an key-value entry of the following style to the first
section: 
```
    pathXXX = /additional/search/path 
```
where the `XXX` part of the key should be an increasing unique integer for
each additional search path. 

If no default mapping is set in one of the configuration files, **FlexiBLAS**
will use the standard *Netlib* implementation. 

The user config files can be handle using the `flexiblas` tool. This tool can
manage the user and the system configuration file, excluding the adding
additional search paths. 

The `flexiblas` tool can be used for example to: 
* List all installed backends and the complete configuration:
```
    flexiblas list 
```

* Set the default backend:
```
    flexiblas default NAME_OF_THE_BACKEND 
```
If the name of the backend is not given, the default setting is removed. 

New *BLAS* backends can also be added to the configuration files using the
`flexiblas` tool. For this please look at the output of 
```
    flexiblas help 
```

**Attention:** In case of a 64-bit integer build the `flexiblas` tool is
renamed to `flexiblas64` in order to setup both, a 32-bit and a 64-bit version
in parallel.

## Selecting the backend at runtime
The behavior of the **FlexiBLAS** library is controlled by a set of environment 
variables described below and in the above mentioned configuration files. The
most important environment variable is called `FLEXIBLAS`. This variable
describes the shared object library file where the backend *BLAS* implementation
is contained. The variable supports three types of descriptions. If the value
does not contain a `/` **FlexiBLAS** searches for the given name in
`CMAKE_INSTALL_PREFIX/lib/flexiblas`. That means the value is interpreted as a
name of one of the wrapper libraries. If it does not find it there, it searches
the configuration files for a line matching the value and fetches the actual 
name of the shared object library from there. Otherwise the value is
interpreted as a full path description of the *BLAS* backend library shared 
object file to use. If the variable is not set, the *Netlib* reference *BLAS*,
or the default from the configuration files is used. See the documentation for 
details on the names, syntax and locations of the configuration files. 

## Usage example
If **FlexiBLAS** is built with an interface to the *ATLAS* library then one can
use the following in an application to call the *ATLAS* instead of the default
*BLAS*: 
```
    FLEXIBLAS="libblas_atlas.so" ./yourapp
```
In case **FlexiBLAS** should use *OpenBLAS* and *OpenBLAS* is contained in one
of the configuration files the application can be stated like:
```
    FLEXIBLAS=OpenBLAS ./yourapp 
```

## Linking a program against **FlexiBLAS**
By default **FlexiBLAS** produces a library called `libflexiblas.so`, which
one can link against a program just like the reference *BLAS* implementation.
Other libraries are not necessary. 

## Environment variables
* `FLEXIBLAS=PATH/TO/BLAS`  
  This is the main variable used by **FlexiBLAS**. It selects the *BLAS* library 
  which is used as backend this should be a shared object which contains or 
  allows to resolve all symbols defined in the *Netlib* reference *BLAS*
  library. The default search path is `CMAKE_INSTALL_PREFIX/lib/flexiblas` or
  `CMAKE_INSTALL_PREFIX/lib/flexiblas-profile`. Alternatively, an absolute
  path to a shared object can be given. If the variable is not set it uses 
  `libblas_netlib.so` as default, or any other backend that is specified as 
  default in the configuration files. 
  For example, if *OpenBLAS* and *ATLAS* are available the following two
  additional implementations can be used:
    * `libblas_openblas.so`  - to select *OpenBLAS*
    * `libblas_atlas.so`     - to select *ATLAS*
  **FlexiBLAS** installs a command `flexiblas` that can be used to find list
  all available backends (`flexiblas list`) and prescribe the users default 
  (`flexiblas set NAME_OF_BACKEND`) 

* `FLEXIBLAS_VERBOSE=0/1`  
  Enables some additional output at the startup of **FlexiBLAS**. This can be 
  useful if a backend is not loaded correctly, or when logfiles should contain 
  the actual *BLAS* backend used in a certain run of the user application, e.g., 
  because several implementations are compared for benchmarking purposes. 
  If the variable is not set, it is assumed to be 0 and the additional output 
  is turned off.

* `FLEXIBLAS_PROFILE=0/1`  
  Turns on profiling. This replaces the `FLEXIBLAS_NOPROFILE` variable. 

* `FLEXIBLAS_PROFILE_FILE=FILENAME`
  Set a filename to write profiling output to. If the variable is set, the
  profiling output will be redirected to this file. Otherwise it is written to
  the standard error output `stderr` which is the default setting if the
  variable is not set. 
  
**Attention:** In the case of a 64-bit integer build, all environment variables 
begin with `FLEXIBLAS64` instead of `FLEXIBLAS`. 

## 64-Bit Integer Build
As already mentioned in the case when the 64-bit integer interface is enabled,
the names of all libraries, directories and environment variables are suffixed
by `64`, that means in order to link against the 64-bit library, you have to
use:
``` 
    gcc -lflexiblas64 ...
``` 
The flexiblas tool is now named `flexiblas64`. This allows to installed both 
variants on a single system. 

## Profiling 
The profiling is compiled into the library. It is enabled using the
`FLEXIBLAS_PROFILE` environment variable. Since **FlexiBLAS** 1.1.0 no separate 
profiling library exists.

## *Octave* Interface 
The *GNU Octave* interface is a separate package which can be downloaded from:
```
    http://www.mpi-magdeburg.mpg.de/projects/flexiblas
```
and installed via:
```
    pkg install -verbose flexiblas-octave.tar.gz 
```
from the commandline prompt in *GNU Octave*. After installing the package you
can get an overview about the installed functions by calling:
```
    pkg describe -verbose flexiblas
``` 
from the *GNU Octave* commandline prompt. The package only works on *Linux* and
*BSD* versions of *GNU Octave*, but **NOT** on any version of *MS-Windows*.

## Acknowledgements
We want to thank Christian Himpe (WWU Muenster) for the testing he has done. 

## License
The whole library is provided under the conditions of the *GPLv3*. 
Except of the testing code ( contained in `test/` ) and the *Netlib BLAS* 
implementation (contained in `netlib/`). These two parts are covered by the 
license of *LAPACK* (http://wwww.netlib.org/lapack). See ``COPYING.NETLIB`` 
for more details about this license.
