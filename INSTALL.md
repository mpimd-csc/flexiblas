# Installation of FlexiBLAS

**Table of Contents**

 * [Requirements](#requirements)
 * [Installation](#installation)
 * [Packaging](#packaging)
 * [Testing](#testing)
 * [Windows Support](#windows-support)
 * [MacOS Support](#macos-support)

## Requirements

FlexiBLAS tries to minimize the build time dependencies as much as possible.
Therefore, for a basic installation, the following requirements must exist on
the system:

 * A C11 compatible C compiler
 * A Fortran 95 compatible Fortran compiler
 * cmake >= 3.15
 * GNU Make or Ninja

The following compilers are tested during the development:

 * GCC >= 9.x
 * Intel icc/ifort Classic 2021
 * Intel icx/ifx oneAPI >= 2022
 * LLVM CLANG/FLANG >= 18
 * AMD AOCC >= 4.2.0
 * NVHPC on `x86_64` (On ARM64 some issues exists.)

Other compilers might work, but not tested.

In addition to the basic requirements, **BLAS** libraries can be installed
beforehand to be detected by `cmake`. At the moment the following ones are
tested during development:

 * *Netlib* [*BLAS*](https://www.netlib.org/blas/) and [*LAPACK*](https://www.netlib.org/lapack/) Reference Implementation
 * [*ATLAS*](http://math-atlas.sourceforge.net/)
 * [*OpenBLAS*](http://www.openblas.net/)
 * [*BLIS*](https://github.com/flame/blis)
 * [*Intel MKL*](https://software.intel.com/content/www/us/en/develop/tools/math-kernel-library.html)



## Installation
The build system used by **FlexiBLAS** is *cmake* which prefers an out-of-source
build:

```shell
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
    integers as well. Since most applications and libraries still use the
    classic 32-bit integers the default is `OFF`.

* `-DCBLAS=ON/OFF`

    Enables the *CBLAS* frontend. If the backend itself contains a *CBLAS*
    interface that one is used. Otherwise, **FlexiBLAS** acts like the
    *CBLAS*-wrapper from *Netlib*. This option is set to `ON` by default.

* `-DABI=GNU|Intel`

    Selects the ABI (Application Binary Interface) of the *Fortran* Interface.
    By default, the compiler used determines which ABI is used, but it can also
    be forced using this parameter. The difference between both ABIs is how
    complex values are returned from Functions. The *GNU*-style returns them as
    a normal return value on the stack. The *Intel* (*F2C*, *GNU F77*) style
    returns them as additional parameters in front of the proper function
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
    between single and multi-threaded *MKL* in your application. Turning this
    option on requires having the `MKLROOT` environment variable set properly
    and an *MKL* version >=11.1. The default is `OFF`.

* `-DDEV=ON/OFF`

    Configures the build to work only in the build directory. In this
    case, **FlexiBLAS** searches in the root of the build directory for its
    configuration file and the build directory is added to the default library
    search path. Turning on this option is only useful during the development in
    order to work independently from an installed **FlexiBLAS**. If you plan to
    install **FlexiBLAS**, set it to `OFF` which is also the default value.

* `-DBLAS_AUTO_DETECT=ON/OFF`

    Enable/disable the automatic search for *BLAS* libraries during
    configuration. They need to be added separately using the `-DEXTRA` option
    or configured afterward. The default value is `ON`.

* `-DLAPACK=ON/OFF`

    Turn `ON`/`OFF` the *LAPACK* support of **FlexiBLAS**. If enabled
    **FlexiBLAS** also includes the wrappers around *LAPACK*. Default value is
    `ON`.

* `-DLAPACK_API_VERSION=VERSION`

    Selects *LAPACK* API compatibility level. By default, the newest one is used.
    If a *LAPACK* library loaded at runtime does not provide all functions and
    subroutines, the missing ones are loaded from the fallback.
    The following *LAPACK* versions are supported:

    | LAPACK_API_VERSION |                                          | Testing |
    |--------------------|------------------------------------------|---------|
    | 3.12.1             | LAPACK 3.12.1                            |   yes   |
    | 3.12.1-wodprc      | LAPACK 3.12.1 without deprecated routines|   yes   |
    | 3.12.0             | LAPACK 3.12.0                            |   yes   |
    | 3.12.0-wodprc      | LAPACK 3.12.0 without deprecated routines|   yes   |
    | 3.11.0             | LAPACK 3.11.0                            |   yes   |
    | 3.11.0-wodprc      | LAPACK 3.11.0 without deprecated routines|   yes   |
    | 3.10.1             | LAPACK 3.10.1                            |   yes   |
    | 3.10.1-wodprc      | LAPACK 3.10.1 without deprecated routines|   yes   |
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

* `-DLAPACKE=ON/OFF`

    Turn `ON`/`OFF` the *LAPACKE* support of **FlexiBLAS**. If enabled
    **FlexiBLAS** also includes the wrappers around *LAPACKE*. Default value is
    `ON`. This option requires `LAPACK=ON` and an API version of at least
    3.6.1

* `-DFLEXIBLAS_DEFAULT=NAME`

    Set the default *BLAS* at compile time. If the option is not set `NETLIB` is
    used by default.

* `-DLINK_OPENMP=OFF/ON`
    Link FlexiBLAS against the compiler's OpenMP implementation. This might be
    necessary if Python/NumPy and OpenMP flags like OMP_PROC_BIND are used. Be
    default this is avoided in order to avoid OpenMP as cross-dependency in
    applications that do not require OpenMP. If the OS does not support the
    `RTLD_NODELETE` flag in its `dlopen` call, setting this option to `ON` can
    be helpful.

* `-DLTO=ON/OFF`
    Enables the Link Time Optimization in the compiler, if supported. By default
    this is enabled.

The `PROFILE=ON/OFF` option was removed from version 1.1.0 onward. Beginning
with version 3.0.0 profiling is done using a hook functionality and is no
longer compiled in **FlexiBLAS** directly. See the Profiling section for
details.

### Setup with precompiled reference BLAS and LAPACK

On many Linux and *BSD* operating systems static versions of *BLAS* and *LAPACK*
can be installed from the package management system. These versions can be used
instead of **FlexiBLAS**'s internal fallback implementation. In order to use a
provided static library providing a *BLAS* API it needs to be ensured that this
library is compiled with support for position-independent code (`-fPIC`).

The *BLAS* implementation is set in the configuration procedure via
```shell
    cmake -DSYS_BLAS_LIBRARY=/absolute/path/to/liblas.a
```
The *BLAS* library has to provide all functions and subroutines available in
the reference *BLAS* implementation since *LAPACK* 3.0.

The *LAPACK* fallback implementation can be provided in the same way via
```shell
cmake -DSYS_LAPACK_LIBRARY=/absolute/path/to/liblapack.a \
      -DLAPACK_API_VERSION="LAPACK-VERSION"
```
The *LAPACK* library must provide all symbols that the reference implementation
from *NETLIB* with the given version provides. A list of supported *LAPACK*
versions are given above.
If the version is not specified *cmake* tries to obtain it with a call to
*LAPACK*'s `ILAVER`. Note that the path must be absolute, the `~`-operator to
reference the home directory is not allowed.

### Setup with Custom *BLAS* and *LAPACK* Implementations.

By default, **FlexiBLAS** tries to locate as many *BLAS* and *LAPACK*
installations as possible on your system. If you want to add our own ones to the
default setup you can pass the `-DEXTRA` option to *cmake*. It specifies a
semicolon-separated list of additional *BLAS* names that should be added. For
each additional library name `name` in the list, a path needs to be specified via
`-Dname_LIBRARY=PATH_TO_NAME`. If special linker flags are required the option
`-Dname_LINKER_FLAGS="FLAGS"` can be passed to *cmake*. For example, if a custom
build *OpenBLAS-0.3.4* and *OpenBLAS-0.3.10* should be included, the following
options have to be added to cmake:
```shell
    -DEXTRA="OpenBLAS034;OpenBLAS0310" \
    -DOpenBLAS034_LIBRARY="/home/user/openblas-0.3.4/libopenblas.a;gomp;pthread;" \
    -DOpenBLAS0310_LIBRARY="/home/user/openblas-0.3.10/libopenblas.a;gomp;pthread;"
```

## Packaging
### Setup on *Debian* and Derivatives

The easiest way is to build a *Debian* package out of the source and install it.
At the moment we support the package creation on *Ubuntu 20.04* and *Debian 11*
(Testing) since they provide *OpenBLAS* and *BLIS* split in different
packages depending on the used threading model. In order to build the package
the following prerequisites need to be installed:
```shell
sudo apt-get install debhelper devscripts cmake gfortran gcc \
    libatlas-base-dev \
	libopenblas-openmp-dev libopenblas-pthread-dev libopenblas-serial-dev \
	libopenblas64-openmp-dev libopenblas64-pthread-dev \
	libopenblas64-serial-dev libblis-openmp-dev libblis-pthread-dev \
	libblis-serial-dev libblis64-openmp-dev libblis64-pthread-dev \
	libblis64-serial-dev libblas-dev libblas64-dev liblapack-dev \
	liblapack64-dev \
    libmkl-dev
```
The `libmkl-dev` packages is only supported on `x86_64` target and not available
on `aarch64`,`ppc64le`, or other non `x86_64` based architectures. The ATLAS
library `libatlas-base-dev` is no longer under development and is removed in
newer version of Debian (beginning with **Trixie**) and Ubuntu. If the package is
not available, you can remove it from the above list.

Next set your information to be included in the package building process:
```shell
export DEB_FULLNAME="Your Name"
export DEBEMAIL="you@example.org"
sh debian/bootstrap_debian.sh
```
Afterwards, you can build the *Debian* package out of the source code by typing:
```shell
dpkg-buildpackage -us -uc --build=binary
```
inside the extracted source. If the source package is also required, use
```shell
dpkg-buildpackage -us -uc --build=full
```
instead. After building the package the source directory can
be cleaned using
```shell
debian/rules clean
```
The build process creates a set of packages that are available one directory
level above. They are installed using:
```shell
sudo dpkg -i ../libflexiblas*.deb
```

If you update from **FlexiBLAS** <= 2.0.0 please remove all related packages and
configuration files before.

### Setup on Fedora
If you are using Fedora Linux, you can install FlexiBLAS easily using `dnf`
since FlexiBLAS is part of Fedora Linux. Just type
```shell
dnf install flexiblas
```
This installs most likely one of the latest FlexiBLAS releases.

Thanks Iñaki Ucar (https://src.fedoraproject.org/user/iucar) for packaging and
maintaining the Fedora integration.



### Caveats

* If **FlexiBLAS** is not automatically selected by the `update-alternatives`
  mechanism you have to select it via (Ubuntu and Debian derivatives):
  ```shell
  sudo update-alternatives --config libblas64.so.3-x86_64-linux-gnu
  sudo update-alternatives --config libblas.so.3-x86_64-linux-gnu
  sudo update-alternatives --config libblas64.so-x86_64-linux-gnu
  sudo update-alternatives --config libblas.so-x86_64-linux-gnu
  ```

  On non-Debian based systems the names may differ. If your are using a
  non-x86_64 architecture, you have to adjust the architecture triplet
  accordingly.

* **FlexiBLAS** requires that you use the reference implementation of *LAPACK*
  that means if `update-alternatives` uses the *LAPACK* implementation provided
  by other BLAS libaries many programs will fail. Please select **FlexiBLAS** as
  well via:
  ```shell
  sudo update-alternatives --config liblapack64.so.3-x86_64-linux-gnu
  sudo update-alternatives --config liblapack.so.3-x86_64-linux-gnu
  sudo update-alternatives --config liblapack64.so-x86_64-linux-gnu
  sudo update-alternatives --config liblapack.so-x86_64-linux-gnu
  ```
  On non-Debian based systems the names may differ. If your are using a
  non-x86_64 architecture, you have to adjust the architecture triplet
  accordingly.

* Due to a bug in the *Ubuntu* *OpenBLAS* packages **FlexiBLAS** does not work
  with the **OpenBLAS** package versions between 0.2.6 and 0.2.8-4. If you have
  one of these versions installed, please update *OpenBLAS* before installing
  **FlexiBLAS**.

* If FlexiBLAS should be used on MacOS X and OpenBLAS is installed via `brew`,
  it is necessary to extend cmake's search path by setting:
  ```shell
  export CMAKE_PREFIX_PATH=/usr/local/opt/openblas:$CMAKE_PREFIX_PATH
  ```

* If the Intel Compiler suite is used, please ensure that either the classic or
  the LLVM based compilers are used for both, C and Fortran.


## Testing
**FlexiBLAS** comes with the reference *BLAS* and reference *LAPACK* test suite.
The tests are executed using
```shell
     make test
```
and use the *NETLIB BLAS* implementation by default in order to check
compliance with the reference implementation. Other *BLAS* and *LAPACK*
implementations can be tested by setting the `FLEXIBLAS_TEST` environment
variable. The variable has the same meaning as the `FLEXIBLAS` environment
variable but only for the tests. If single tests need to be selected we refer to
the help of the *cmake* `ctest` utility. Running the tests with other backends
then the NETLIB one, is not recommended.

## Windows Support

In general FlexiBLAS supports Microsoft Windows as well, but this support is
experimental supporting issues can not be guaranteed. At the moment the
support is limited to the Microsoft Visual Studio >= 2019 together with the
Intel C/C++ and Fortran compilers.

## MacOS Support

Since MacOS is a Unix based and POSIX compatible operating system, FlexiBLAS can
be used there as well. Since the native XCode lacks OpenMP and Fortran support,
FlexiBLAS required to be built with GCC. Currently, the gcc from homebrew is
known to work. Mixing MacOS's CLANG with gfortran is not recommended. Tests will
not work in this case.

