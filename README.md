FlexiBLAS - A BLAS and LAPACK wrapper library with runtime exchangeable backends
================================================================================

**Version 3.4.80** - DOI: 10.5281/zenodo.14764701

**Project Website:** https://www.mpi-magdeburg.mpg.de/projects/flexiblas

**Git Repository:** https://gitlab.mpi-magdeburg.mpg.de/software/flexiblas-release

Copyright 2013-2025 by *Martin Köhler* (ORCID: 0000-0003-2338-9904)
                   and *Jens Saak* (ORCID: 0000-0001-5567-9637)

## Documentation

[This Documentation includes](#documentation):

  * [About FlexiBLAS](#about-flexiblas)
  * [Installing FlexiBLAS](#install)
  * [Linking with FlexiBLAS](#linking-a-program-against-flexiblas)
  * [Setting up FlexiBLAS](#configuration-files)
  * [Runtime usage of FlexiBLAS](#selecting-the-backend-at-runtime)
  * [Usage Examples](#usage-examples)
  * [Environment Variables](#environment-variables)
  * [Profiling with FlexiBLAS](#profiling)
  * [Octave Interface](#octave-interface)
  * [Backend Builder](#backend-builder)

## About Flexiblas

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

See [INSTALL.md](./INSTALL.md).

## Linking a Program Against **FlexiBLAS**

By default **FlexiBLAS** produces a library called `libflexiblas.so`, which
one can link against a program just like the reference *BLAS* implementation.
Other libraries are not necessary.

### 64-Bit Integer Build

As already mentioned in the case when the 64-bit integer interface is enabled,
the names of all libraries, directories, and environment variables are suffixed
by `64`, that means in order to link against the 64-bit library, you have to
use:
```shell
gcc -lflexiblas64 ...
```
The flexiblas tool is now named `flexiblas64`. This allows to installed of both
variants on a single system.



## Configuration Files

**FlexiBLAS** reads a set of configuration files at startup. These are:
* `CMAKE_INSTALL_PREFIX/etc/flexiblasrc`
* `CMAKE_INSTALL_PREFIX/etc/flexiblasrc.d/*.conf`
* `${HOME}/.flexiblasrc`
* `${HOME}/.flexiblasrc.$(hostname)`
* `${FLEXIBLAS_CONFIG}`

If **FlexiBLAS** is compiled with the `INTEGER8` option enabled, `flexiblasrc`
is replaced by `flexiblasrc64`.
```shell
CMAKE_INSTALL_PREFIX/etc/flexiblasrc
```
and:
```shell
~/.flexiblasrc
```
or:
```shell
CMAKE_INSTALL_PREFIX/etc/flexiblasrc64
```
and:
```shell
~/.flexiblasrc64
```
in case of a 64-bit integer build.

The files contain a mapping between the name of a backend and the corresponding
name of the shared object. The setting in the user-owned file overrides the
global settings. All settings are superseded eventually by setting environment
variables. The syntax of the file is similar to the *KDE* configuration files or
the *ini*-files known from early *MS-Windows* versions. Basically, it is a
key-value store with section support. In the first section of the configuration
file are global definitions like the default *BLAS* backend or the verbosity
level:
```
default = NETLIB
verbose = 0
```
Then for each *BLAS* backend, there is a block of the following style:
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
Therefore one has to add a key-value entry of the following style to the first
section:
```
pathXXX = /additional/search/path
```
where the `XXX` part of the key should be an increasing unique integer for
each additional search path.

If no default mapping is set in one of the configuration files, **FlexiBLAS**
will use the standard *Netlib* implementation.

The user config files can be handled using the `flexiblas` tool. This tool can
manage the user and the system configuration file, excluding adding
additional search paths.

The `flexiblas` tool can be used for example to:
* List all installed backends and the complete configuration:
  ```shell
  flexiblas list
  ```
* Set the default backend:
  ```shell
  flexiblas default NAME_OF_THE_BACKEND
  ```
If the name of the backend is not given, the default setting is removed.

New *BLAS* backends can also be added to the configuration files using the
`flexiblas` tool. For this please look at the output of
```shell
flexiblas help
```

**Caution:** In case of a 64-bit integer build the `flexiblas` tool is
renamed to `flexiblas64` in order to setup both, a 32-bit and a 64-bit version
in parallel.

## Selecting the Backend at Runtime

The behavior of the **FlexiBLAS** wrapper is controlled by a set of environment
variables described below and in the above-mentioned configuration files. The
most important environment variable is called `FLEXIBLAS`. This variable
describes the shared object library file where the backend *BLAS* implementation
is contained. The variable supports three types of descriptions. If the value
does not contain a `/` **FlexiBLAS** searches for the given name in
`CMAKE_INSTALL_PREFIX/lib/flexiblas`. That means the value is interpreted as the
name of one of the wrapper libraries. If it does not find it there, it searches
the configuration files for a line matching the value and fetches the actual
name of the shared object library from there. Otherwise, the value is
interpreted as a full path description of the *BLAS* backend library shared
object file to use. If the variable is not set, the *Netlib* reference *BLAS*,
or the default from the configuration files is used. See the documentation for
details on the names, syntax, and locations of the configuration files.

## Usage Example

If **FlexiBLAS** is built with an interface to the *ATLAS* library then one can
use the following in an application to call the *ATLAS* instead of the default
*BLAS*:
```shell
FLEXIBLAS="libblas_atlas.so" ./yourapp
```
In case **FlexiBLAS** should use *OpenBLAS* and is registered as `OpenBLAS` in
one of the configuration files the application can be started as follows:
```shell
FLEXIBLAS=OpenBLAS ./yourapp
```

Remember you can list all installed backends with:
```shell
flexiblas list
```

## Environment Variables

* `FLEXIBLAS=PATH/TO/BLAS`

    This is the main variable used by **FlexiBLAS**. It selects the *BLAS*
    library which is used as the backend. This should be a shared object which
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
    **FlexiBLAS** installs a command `flexiblas` that can be used to find a list
    of all available backends (`flexiblas list`) and prescribe the users default
    (`flexiblas set NAME_OF_BACKEND`)

* `FLEXIBLAS_VERBOSE=LEVEL`

    Enables some additional output at the startup of **FlexiBLAS**. This can be
    useful if a backend is not loaded correctly, or when log files should
    contain the actual *BLAS* backend used in a certain run of the user
    application, e.g., because several implementations are compared for
    benchmark purposes. If the variable is not set, it is assumed to be `0`
    and the additional output is turned off.

* `FLEXIBLAS_NOLAPACK=0/1`

    If set to one, **FlexiBLAS** does not load *LAPACK* from the backends. Only
    the internal fallback is used. In this way detailed profiling and logging
    is possible. The default value is `0`.

* `FLEXIBLAS_COLOR_OUTPUT=0/1`

    Enable / Disable colored output in verbose mode. The default value is `1`.

* `FLEXIBLAS_CONFIG=PATH`

    Specify a `flexiblasrc` config file. The settings in the file overwrite all
    setting in the system/user/host config file.

* `FLEXIBLAS_LIBRARY_PATH=PATHS`

    Add additional paths, in a colon-separated list, to the search paths of the
    **FlexiBLAS** library. The paths are added in front of the search list.

**Attention:** In the case of a 64-bit integer build, all environment variables
begin with `FLEXIBLAS64` instead of `FLEXIBLAS`.

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
    already another hook is loaded, The profile library can be chain-loaded by
    adding it separated by a colon from the other hook libraries.

* `FLEXIBLAS_PROFILE_FILE=FILENAME`

    Sets a filename to write profiling output to. If the variable is set, the
    profiling output will be redirected to this file. Otherwise, it is written to
    the standard error output `stderr` which is the default setting if the
    variable is not set.

## *Octave* Interface

The *GNU Octave* interface is a separate package that can be downloaded from:

http://www.mpi-magdeburg.mpg.de/projects/flexiblas

and installed via:

        pkg install -verbose flexiblas-octave.tar.gz

from the command-line prompt in *GNU Octave*. After installing the package you
can get an overview of the installed functions by calling:

        pkg describe -verbose flexiblas

from the *GNU Octave* command-line prompt. This package only works on *Linux*
and *BSD* versions of *GNU Octave*, but **NOT** on any version of *MS-Windows*!

## Backend Builder

In order to build backends later without reconfiguring or rebuilding FlexiBLAS,
a CMake helper tool exists in `tools/backend_builder`. This tool can be used to
build your own backends individually. Normally, this tool is not required, for
single-file BLAS implementations, like OpenBLAS, BLIS,... But in case of
libraries like Intel (oneAPI) MKL, this can be helpful.

## Versioning
FlexiBLAS uses semantic versioning, with an exception. If the patch level in
`MAJOR.MINOR.PATCHLEVEL` is greater than or equal to 80, this is an alpha/beta/
release candidate of the next FlexiBLAS minor or major release.

## Cite As

M. Köhler, J. Saak. **FlexiBLAS - A flexible BLAS library with runtime
exchangeable backends**. LAPACK Working Note: 284, 2013.
https://www.netlib.org/lapack/lawnspdf/lawn284.pdf

## Acknowledgments

We thank *Christian Himpe* (0000-0003-2194-6754) for documenting, testing, and
providing feedback.

We also thank *Iñaki Ucar* (0000-0001-6403-5550) for creating a *Fedora* package
and pointing out Linux software maintainer-related issues with the configuration
and building process.

## Bug Reports

If you find a bug in **FlexiBLAS** please prepare a minimal examples which
reproduces the bug. Without minimal examples, we cannot fix the bug.
Send feedback to:

&#x6b;&#x6f;&#x65;&#x68;&#x6c;&#x65;&#x72;&#x6d;&#x40;&#x6d;&#x70;&#x69;&#x2d;&#x6d;&#x61;&#x67;&#x64;&#x65;&#x62;&#x75;&#x72;&#x67;&#x2e;&#x6d;&#x70;&#x67;&#x2e;&#x64;&#x65;

## License

The whole library is provided under the conditions of the *LGPLv3* and later
The testing code ( contained in `test/` ) and the *Reference Implementation*
implementation (contained in `contributed/`) are covered by the
license of *LAPACK* ( https://www.netlib.org/lapack ). See ``COPYING.NETLIB``
for more details about this license.
