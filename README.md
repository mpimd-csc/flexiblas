FlexiBLAS - A BLAS and LAPACK wrapper library with runtime exchangeable backends
================================================================================

**Version 3.1.0** https://doi.org/10.5281/zenodo.3949804

**Project Website:** https://www.mpi-magdeburg.mpg.de/projects/flexiblas

**Git Repository:** https://gitlab.mpi-magdeburg.mpg.de/software/flexiblas-release

Copyright 2013-2022 by *Martin Köhler* (0000-0003-2338-9904)
                   and *Jens Saak* (0000-0001-5567-9637)

**FlexiBLAS** is a wrapper library that enables the exchange of the
[*BLAS*](https://en.wikipedia.org/wiki/BLAS) (Basic Linear Algebra System) and
[*LAPACK*](https://en.wikipedia.org/wiki/LAPACK) (Linear Algebra PACKage)
implementation used in an executable without recompiling or re-linking it. This
could in principle also be achieved using the `LD_LIBRARY_PATH` mechanism, if
all *BLAS* and *LAPACK* were consistently implemented as a single library file
with a fully compatible interface. Unfortunately, not all *BLAS* and
*LAPACK* libraries are realized as only this exact one shared library
containing all required symbols. Implementations with differing numbers of
library files (shared objects) clearly break this mechanism of easily switching
via the `LD_LIBRARY_PATH` environment variable.

The **FlexiBLAS** library provides a *GNU Fortran* compatible interface to all
functions and subroutines provided by the *Netlib* reference implementations. As
backends **FlexiBLAS** can employ all *BLAS* and *LAPACK* implementations which
consist of a single shared library directly. Other variants like the *Intel MKL*
or *ATLAS* that use multiple files are integrated by *FlexiBLAS* by wrapping all
files into a single surrogate library.

## Install

The build system used by **FlexiBLAS** is *cmake* which prefers an out-of-source
build:

        mkdir build
        cd build
        cmake ../ <FURTHEROPTIONS>
        make
        make install

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

    Selects the ABI (Application Binary Interface) of the *Fortran* Interface.
    By default the compiler used determines which ABI is used, but it can also
    be forced using this parameter. The difference between both ABIs is how
    complex values are returned from Functions. The *GNU*-style returns them as
    a normal return value on the stack. The *Intel* (*F2C*, *GNU F77*) style
    returns them as an additional parameters in front of the proper function
    arguments. That means it is a subroutine from the *C* point of view.

* `-DBLASNAME=OFF`

    Deactivate the automatic search for the *BLAS* library with the name
    **BLASNAME**.

* `-DEXTRA=List`

    Add an additional list of *BLAS* libraries to the default setup of
    **FlexiBLAS**. See below for details.

* `-DMKL_BUILDER=ON/OFF`

    Turn `ON`/`OFF` the build of an *Intel MKL* custom library instead of
    searching for the *MKL* directly. This is necessary if you plan to switch
    between single and multithreaded *MKL* in your application. Turning this
    option on requires to have the `MKLROOT` environment variable set properly
    and an *MKL* version >=11.1. The default is `OFF`.

* `-DDEV=ON/OFF`

    Configures the build to work only in the build directory. In this
    case **FlexiBLAS** searches in the root of the build directory for its
    configuration file and the build directory is added to the default library
    search path. Turning on this option is only useful during the development in
    order to work independently from an installed **FlexiBLAS**. If you plan to
    install **FlexiBLAS**, set it to `OFF` which is also the default value.

* `-DBLAS_AUTO_DETECT=ON/OFF`

    Enable/disable the automatic search for *BLAS* libraries during
    configuration. They need to be added separately using the `-DEXTRA` option
    or configured afterwards. The default value is `ON`.

* `-DLAPACK=ON/OFF`

    Turn `ON`/`OFF` the *LAPACK* support of **FlexiBLAS**. If enabled
    **FlexiBLAS** also includes the wrappers around *LAPACK*. Default value is
    `ON`.

* `-DLAPACK_API_VERSION=VERSION`

    Selects *LAPACK* API compatibility level. By default the newest one is used.
    If a *LAPACK* library loaded at runtime does not provide all functions and
    subroutines, the missing ones are loaded from the fallback.
    The following *LAPACK* versions are supported:

    | LAPACK_API_VERSION |                                          | Testing |
    |--------------------|------------------------------------------|---------|
    | 3.10.0             | LAPACK 3.10.0                            |   yes   |
    | 3.10.0-wodprc      | LAPACK 3.10.0 without deprecated routines|   yes   |
    | 3.9.1              | LAPACK 3.9.1                             |   yes   |
    | 3.9.1-wodprc       | LAPACK 3.9.1 without deprecated routines |   yes   |
    | 3.9.0              | LAPACK 3.9.0                             |   yes   |
    | 3.9.0-wodprc       | LAPACK 3.9.0 without deprecated routines |   yes   |
    | 3.8.0              | LAPACK 3.8.0                             |   yes   |
    | 3.8.0-wodprc       | LAPACK 3.8.0 without deprecated routines |   yes   |
    | 3.7.1              | LAPACK 3.7.1                             |   yes   |
    | 3.7.1-wodprc       | LAPACK 3.7.1 without deprecated routines |   yes   |
    | 3.7.0              | LAPACK 3.7.0                             |   yes   |
    | 3.7.0-wodprc       | LAPACK 3.7.0 without deprecated routines |   yes   |
    | 3.6.1              | LAPACK 3.6.1                             |   yes   |
    | 3.6.1-wodprc       | LAPACK 3.6.1 without deprecated routines |   yes   |
    | 3.6.0              | LAPACK 3.6.0                             |   no    |
    | 3.6.0-wodprc       | LAPACK 3.6.0 without deprecated routines |   no    |
    | 3.5.0              | LAPACK 3.5.0                             |   no    |
    | 3.4.2              | LAPACK 3.4.2                             |   no    |
    | 3.4.1              | LAPACK 3.4.1                             |   no    |
    | 3.4.0              | LAPACK 3.4.0                             |   no    |
    | 3.3.1              | LAPACK 3.3.1                             |   no    |
    | 3.3.0              | LAPACK 3.3.0                             |   no    |

* `-DFLEXIBLAS_DEFAULT=NAME`

    Set the default *BLAS* at compile time. If the option is not set `NETLIB` is
    used by default.

* `-DLINK_OPENMP=OFF/ON`
    Link FlexiBLAS against the compiler's OpenMP implementation. This might be
    necessary if Python/NumPy and OpenMP flags like OMP_PROC_BIND are used. Be
    default this is avoided in order to avoid OpenMP as cross dependency in
    application that do not required OpenMP. If the OS does not support the
    `RTLD_NODELETE` flag in its `dlopen` call, setting this option to `ON` can
    be helpful.

The `PROFILE=ON/OFF` option was removed from version 1.1.0 onwards. Beginning
with version 3.0.0 profiling is done using a hook functionality and is not
longer compiled in **FlexiBLAS** directly. See the Profiling section for
details.

The following *BLAS* backends are tested during development:

* *Netlib* [*BLAS*](https://www.netlib.org/blas/) and [*LAPACK*](https://www.netlib.org/lapack/) Reference Implementation
* [*ATLAS*](http://math-atlas.sourceforge.net/)
* [*OpenBLAS*](http://www.openblas.net/)
* [*BLIS*](https://github.com/flame/blis)
* [*Intel MKL*](https://software.intel.com/content/www/us/en/develop/tools/math-kernel-library.html)

### Setup with precompiled reference BLAS and LAPACK

On many Linux and *BSD* operating systems static versions of *BLAS* and *LAPACK*
can be installed from the package management system. These versions can be used
instead of **FlexiBLAS**'s internal fallback implementation. In order to use a
provided static library providing a *BLAS* API it needs to be ensured that this
library is compiled with support for position independent code (`-fPIC`).

The *BLAS* implementation is set in the configuration procedure via
```
    cmake -DSYS_BLAS_LIBRARY=/absolute/path/to/liblas.a
```
The *BLAS* library have to provided all functions and subroutines available in
the reference *BLAS* implementation since *LAPACK* 3.0.

The *LAPACK* fallback implementation can be provided in the same way via
```
    cmake -DSYS_LAPACK_LIBRARY=/absolute/path/to/liblapack.a \
          -DLAPACK_API_VERSION="LAPACK-VERSION"
```
The *LAPACK* library must provide all symbols that the reference implementation
from *NETLIB* with the given version provides. A list of supported *LAPACK*
versions is given above.
If the version is not specified *cmake* tries to obtain it with a call to
*LAPACK*'s `ILAVER`. Note that the path must be absolute, the `~`-operator to
reference the home directory is not allowed.

### Setup with Custom *BLAS* and *LAPACK* Implementations.

By default, **FlexiBLAS** tries to locate as many *BLAS* and *LAPACK*
installations as possible on your system. If you want to add our own ones to the
default setup you can pass the `-DEXTRA` option to *cmake*. It specifies a
semicolon separated list of additional *BLAS* names that should be added. For
each addtional library name `name` in the list, a path needs to be specified via
`-Dname_LIBRARY=PATH_TO_NAME`. If special linker flags are required the option
`-Dname_LINKER_FLAGS="FLAGS"` can be passed to *cmake*. For example, if a custom
build *OpenBLAS-0.3.4* and *OpenBLAS-0.3.10* should be included, the following
options have to be added to cmake:
```
    -DEXTRA="OpenBLAS034;OpenBLAS0310" \
    -DOpenBLAS034_LIBRARY="/home/user/openblas-0.3.4/libopenblas.a;gomp;pthread;" \
    -DOpenBLAS0310_LIBRARY="/home/user/openblas-0.3.10/libopenblas.a;gomp;pthread;"
```

### Setup on *Debian* and Derivates

The easiest way is to build a *Debian* package out of the source and install it.
At the moment we support the package creation on *Ubuntu 20.04* and *Debian 11*
(Testing) since they provide *OpenBLAS* and *BLIS* split in different
packages depending on the used threading model. In order to build the package
the following prerequisites need to be installed:
```
    sudo apt-get install debhelper cmake gfortran gcc libatlas-base-dev \
	libopenblas-openmp-dev libopenblas-pthread-dev libopenblas-serial-dev \
	libopenblas64-openmp-dev libopenblas64-pthread-dev \
	libopenblas64-serial-dev libblis-openmp-dev libblis-pthread-dev \
	libblis-serial-dev libblis64-openmp-dev libblis64-pthread-dev \
	libblis64-serial-dev libblas-dev libblas64-dev liblapack-dev \
	liblapack64-dev
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
    sudo dpkg -i ../libflexiblas*.deb
```

If you update from **FlexiBLAS** <= 2.0.0 please remove all related packages and
configuration files before.

### Caveats

* If **FlexiBLAS** is not automatically selected by the `update-alternatives`
  mechanism you have to select it via:

        sudo update-alternatives --config libblas.so
        sudo update-alternatives --config libblas.so.3
        sudo update-alternatives --config libblas.so.3gf

    The last line is only necessary up to *Ubuntu 12.04* and *Debian 6*.

* **FlexiBLAS** requires that you use the reference implementation of *LAPACK*
  that means if `update-alternatives` uses the *LAPACK* implementation provided
  by *ATLAS* many programs will fail. Please install the reference *LAPACK*
  package and set it in the `update-alternatives` mechanism via

        sudo update-alternatives --config liblapack.so
        sudo update-alternatives --config liblapack.so.3
        sudo update-alternatives --config liblapack.so.3gf

  The last one is only necessary up to *Ubuntu 12.04* and *Debian 6*.

* Due to a bug in the *Ubuntu* *OpenBLAS* packages **FlexiBLAS** does not work
  with the **OpenBLAS** package versions between 0.2.6 and 0.2.8-4. If you have
  one of these version installed, please update *OpenBLAS* before installing
  **FlexiBLAS**.

### Testing
**FlexiBLAS** comes with the reference *BLAS* and reference *LAPACK* test suite.
The tests are executed using
```
     make test
```
and use the *NETLIB BLAS* implementation by default in order to check the
compliance with the reference implementation. Other *BLAS* and *LAPACK*
implementations can be tested by setting the `FLEXIBLAS_TEST` environment
variable. The variable has the same meaning as the `FLEXIBLAS` environment
variable but only for the tests. If single tests need to be selected we refer to
the help of the *cmake* `ctest` utility.


## Configuration Files

**FlexiBLAS** reads a set of configuration files at startup. These are:
* `CMAKE_INSTALL_PREFIX/etc/flexiblasrc`
* `CMAKE_INSTALL_PREFIX/etc/flexiblasrc.d/*.conf`
* `${HOME}/.flexiblasrc`
* `${HOME}/.flexiblasrc.$(hostname)`
* `${FLEXIBLAS_CONFIG}`

If **FlexiBLAS** is compiled with the `INTEGER8` option enabled, `flexiblasrc`
is replaced by `flexiblasrc64`.

        CMAKE_INSTALL_PREFIX/etc/flexiblasrc

and:

        ~/.flexiblasrc

or:

        CMAKE_INSTALL_PREFIX/etc/flexiblasrc64

and:

        ~/.flexiblasrc64

in case of a 64-bit integer build.

The files contain a mapping between the name of a backend and the corresponding
name of the shared object. The setting in the user owned file overrides the
global settings. All settings are superseded eventually by setting environment
variables. The syntax of the file is similar to the *KDE* configuration files or
the *ini*-files known from early *MS-Windows* versions. Basically it is a
key-value store with section support. In the first section of the configuration
file are global definition like the default *BLAS* backend or the verbosity
level:

        default = NETLIB
        verbose = 0

Than for each *BLAS* backend there is a block of the following style:

        [NAME_OF_THE_BACKEND]
        library = shared_object.so
        comment = Information text about the BLAS backend

If the library argument defines a relative path, **FlexiBLAS** searches inside
its default path (`CMAKE_INSTALL_PREFIX/lib/flexiblas` or
`CMAKE_INSTALL_PREFIX/lib/flexiblas64`) for this shared object. If the path is
absolute it tries to open the library directly. The name of the backend is
case-insensitive!

Additionally, one can define additional search paths in the first section.
Therefore one has to add an key-value entry of the following style to the first
section:

        pathXXX = /additional/search/path

where the `XXX` part of the key should be an increasing unique integer for
each additional search path.

If no default mapping is set in one of the configuration files, **FlexiBLAS**
will use the standard *Netlib* implementation.

The user config files can be handle using the `flexiblas` tool. This tool can
manage the user and the system configuration file, excluding the adding
additional search paths.

The `flexiblas` tool can be used for example to:
* List all installed backends and the complete configuration:

        flexiblas list

* Set the default backend:

        flexiblas default NAME_OF_THE_BACKEND

If the name of the backend is not given, the default setting is removed.

New *BLAS* backends can also be added to the configuration files using the
`flexiblas` tool. For this please look at the output of

        flexiblas help


**Caution:** In case of a 64-bit integer build the `flexiblas` tool is
renamed to `flexiblas64` in order to setup both, a 32-bit and a 64-bit version
in parallel.

## Selecting the Backend at Runtime

The behavior of the **FlexiBLAS** wrapper is controlled by a set of environment
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

## Usage Example

If **FlexiBLAS** is built with an interface to the *ATLAS* library then one can
use the following in an application to call the *ATLAS* instead of the default
*BLAS*:

        FLEXIBLAS="libblas_atlas.so" ./yourapp

In case **FlexiBLAS** should use *OpenBLAS* and is registered as `OpenBLAS` in
one of the configuration files the application can be stated like:

        FLEXIBLAS=OpenBLAS ./yourapp

## Linking a Program Against **FlexiBLAS**

By default **FlexiBLAS** produces a library called `libflexiblas.so`, which
one can link against a program just like the reference *BLAS* implementation.
Other libraries are not necessary.

## Environment Variables

* `FLEXIBLAS=PATH/TO/BLAS`

    This is the main variable used by **FlexiBLAS**. It selects the *BLAS*
    library which is used as backend this should be a shared object which
    contains or allows to resolve all symbols defined in the *Netlib* reference
    *BLAS* library. The default search path is
    `CMAKE_INSTALL_PREFIX/lib/flexiblas` or
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

* `FLEXIBLAS_VERBOSE=LEVEL`

    Enables some additional output at the startup of **FlexiBLAS**. This can be
    useful if a backend is not loaded correctly, or when log files should
    contain the actual *BLAS* backend used in a certain run of the user
    application, e.g., because several implementations are compared for
    benchmarking purposes. If the variable is not set, it is assumed to be `0`
    and the additional output is turned off.

* `FLEXIBLAS_NOLAPACK=0/1`

    If set to one, **FlexiBLAS** does not load *LAPACK* from the backends. Only
    the interal fallback is used. In this way a detailed profiling and logging
    is possible. The default value is `0`.

* `FLEXIBLAS_COLOR_OUTPUT=0/1`

    Enable / Disable colored output in verbose mode. The default value is `1`.

* `FLEXIBLAS_CONFIG=PATH`

    Specify a `flexiblasrc` config file. The settings in the file overwrite all
    setting in the system/user/host config file.

* `FLEXIBLAS_LIBRARY_PATH=PATHS`

    Add additional paths, in a colon separated list, to the search paths of the
    **FlexiBLAS** library. The paths are added in front of the search list.

**Attention:** In the case of a 64-bit integer build, all environment variables
begin with `FLEXIBLAS64` instead of `FLEXIBLAS`.

## 64-Bit Integer Build

As already mentioned in the case when the 64-bit integer interface is enabled,
the names of all libraries, directories and environment variables are suffixed
by `64`, that means in order to link against the 64-bit library, you have to
use:

        gcc -lflexiblas64 ...

The flexiblas tool is now named `flexiblas64`. This allows to installed both
variants on a single system.

## Profiling

Since version 3.0.0 the profiling framework is no longer compiled into the
library. It is now realized using the function overloading support of
**FlexiBLAS**. The profiling is enabled by loading
`libflexiblas_hook_profile.so` in the for function overloading. This is done
using the `FLEXIBLAS_HOOK` environment variable before starting a problem.

The profiling output can be controlled using the following environment
variables:

* `FLEXIBLAS_HOOK=libflexiblas_hook_profile.so`

    Loads the profile library and installs the functions hooks for it. If
    already an other hook is loaded, The profile library can be chain-loaded by
    adding it separated by a colon from the other hook libraries.

* `FLEXIBLAS_PROFILE_FILE=FILENAME`

    Sets a filename to write profiling output to. If the variable is set, the
    profiling output will be redirected to this file. Otherwise it is written to
    the standard error output `stderr` which is the default setting if the
    variable is not set.

## *Octave* Interface

The *GNU Octave* interface is a separate package which can be downloaded from:

http://www.mpi-magdeburg.mpg.de/projects/flexiblas

and installed via:

        pkg install -verbose flexiblas-octave.tar.gz

from the command-line prompt in *GNU Octave*. After installing the package you
can get an overview about the installed functions by calling:

        pkg describe -verbose flexiblas

from the *GNU Octave* command-line prompt. This package only works on *Linux*
and *BSD* versions of *GNU Octave*, but **NOT** on any version of *MS-Windows*!

## Backend Builder

In order to build backends later without reconfiguring or rebuilding FlexiBLAS,
a CMake helper tool exists in `tools/backend_builder`. This tool can be used to
build your own backends individually.

## Cite As

M. Köhler, J. Saak. **FlexiBLAS - A flexible BLAS library with runtime
exchangeable backends**. LAPACK Working Note: 284, 2013.
https://www.netlib.org/lapack/lawnspdf/lawn284.pdf

## Acknowledgments

We thank *Christian Himpe* (0000-0003-2194-6754) for documenting, testing, and
providing feedback.

We also thank *Iñaki Ucar* (0000-0001-6403-5550) for creating a *Fedora* package
and pointing out Linux software maintainer related issues with the configuration
and building process.

## Bug Reports

If you find a bug in **FlexiBLAS** please prepare a minimal examples which
reproduces the bug. Without a minimal examples we cannot fix the bug.
Send feedback to:

&#x6b;&#x6f;&#x65;&#x68;&#x6c;&#x65;&#x72;&#x6d;&#x40;&#x6d;&#x70;&#x69;&#x2d;&#x6d;&#x61;&#x67;&#x64;&#x65;&#x62;&#x75;&#x72;&#x67;&#x2e;&#x6d;&#x70;&#x67;&#x2e;&#x64;&#x65;

## License

The whole library is provided under the conditions of the *GPLv3* and later with
a linking exception according to section 7 of the GNU General Public License,
version 3 ("GPLv3"). This exception, written in COPYING.ADDITIONAL, is valid
for all files in the src/ directory. It allows to link FlexiBLAS against all
software, opensource or proprietary, without violating the GPL license as long
as only the BLAS/LAPACK interface of the reference implementation is used.
The testing code ( contained in `test/` ) and the *Reference Implementation*
implementation (contained in `contributed/`) are covered by the
license of *LAPACK* ( https://www.netlib.org/lapack ). See ``COPYING.NETLIB``
for more details about this license.
