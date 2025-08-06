Issues using FlexiBLAS
======================

Last Update: Oct 10, 2024

Table of Contents
-----------------

1. Profiling Numpy/Scipy with linked against FlexiBLAS
2. CBLAS Tests with ATLAS and MKL
3. Building software with FlexiBLAS
4. FlexiBLAS-API is not linked.
5. Crash with LAPACK interface on IBM Power
6. CBLAS Interface of BLIS
7. Building with pre-built BLAS and LAPACK on MacOSX
8. gcc >= 10.0 and LAPACK 3.7/3.8
9. LTO Type mismatch with gcc
10. NVHPC
11. MacOS X on Apple Silicon

1. Profiling Numpy/Scipy with linked against FlexiBLAS
------------------------------------------------------
Due to the default behaviour of Python to open C-API modules with LAZY and local
binding, not all FlexiBLAS related symbols are integrated correctly in the
current address space. The profiling will not count all function calls
correctly. Many of them will be passed directly to the loaded backend bypassing
the wrapper interface. Changing Python's default behaviour using
sys.setdlopenflags may help in this case but it is not guaranteed.

Solution: Compile and link Numpy/SciPy and all related packages against
fleixblas-profile and load flexiblas-profile again via LD_PRELOAD.


2. CBLAS Tests with ATLAS and MKL
---------------------------------

Due to some diffuclt internals of ATLAS and MKL it is not possible to call
'make test' for the CBLAS interface if ATLAS or MKL is used as BLAS backend.

Solution: Select Netlib or OpenBLAS to perform those tests.


3. Building software with FlexiBLAS
-----------------------------------

The configuration systems GNU autotools and CMake sometimes perform some strange
tests to check if the BLAS libraries are working correctly and if they support
64 bit integers or not. Therefore it is the best to use the NETLIB backend
druing the configuration. This can be easily achieved by setting the FLEXIBLAS
or the FLEXIBLAS64 environment variable to "NETLIB" before starting the
configuration process.

4. FlexiBLAS-API is not linked
------------------------------

If the FlexiBLAS-API library is used and the BLAS library is FlexiBLAS than
libflexiblas_api is not linked against your code if flexiblas_api comes
after flexiblas in the linker command line. The correct order is

    gcc .... -lflexiblas_api -lflexiblas ...

or in general

    gcc .... -lflexiblas_api -lblas ...

in order to avoid problems when switching the BLAS library via `update-alternatives`
or similar tool. The reason for this behavior is that the flexiblas_api library
and the flexiblas library have some common symbols which do not get resolved if
flexiblas_api appear after flexiblas in the linking order.

5. Crash with LAPACK interface on IBM Power
-------------------------------------------
If FlexiBLAS is compiled on the ppc64 or ppc64le with the help of the gcc
compiler (up to version 5.4.0 and 6.3.0) most likely it will crash when calling
an LAPACK function. The reason behind this is a code generation bug inside many
version of gcc. It prevents that recursive function calls work over shared
object boundaries. More detailed information about this bug can be found in the
following bug tracker entries:
* https://bugzilla.redhat.com/show_bug.cgi?id=1420723
* https://gcc.gnu.org/bugzilla/show_bug.cgi?id=79439
* https://sourceware.org/bugzilla/show_bug.cgi?id=21116

There are two solution for this problem. On the one hand one can use the PGI
Compiler Suite for OpenPOWER (the community edition is available at no cost)
and compile FlexiBLAS using the PGI Compiler. This compiler correctly handles
the recursive calls.

Otherwise one can use an gcc from svn. The svn revision need to be at least
r245930. In the upcoming gcc versions this problem will be fixed.

6. CBLAS Interface of BLIS
--------------------------
The CBLAS wrapper in the BLIS library contains a call to `exit` in its
cblas_xerbla implementation. This might alter the behavoir of a program and let
the CBLAS tests fail. For this reason FlexiBLAS does not load the BLIS-CBLAS
interface if it detects the BLIS library.

7. Building with pre-built BLAS and LAPACK on MacOSX
----------------------------------------------------
The `ilaenv` and `iparam2stage` routine from the reference LAPACK implementation
can cause an faulty memory access due to a copy operation with changing
array length. The LAPACK/BLAS reference BLAS and LAPACK implementation shipped
with FlexiBLAS fixes this error. On other systems this error was not detectable
yet.

8. gcc >= 10.0 and LAPACK 3.7/3.8
---------------------------------
The testing code for LAPACK 3.7.x and 3.8.0 contains some legacy Fortran codes
that require the `-fallow-argument-mismatch` flag on recent GNU Fortran version.
Beginning with gcc/gfortran 10.1.0 this flags needs to be set via
```
cmake ........ -DCMAKE_Fortran_FLAGS="-fallow-argument-mismatch"
```
while configuring FlexiBLAS. Alternatively, the tests can be disabled.

9. LTO Type mismatch with gcc
-----------------------------
Some newer versions of gcc (at least version 13.x and 14.x) produce wrong LTO
warnings. They accidently assume Fortran's hidden string length argument to be a
`long int` instead of a `size_t` aka `flexiblas_fortran_charlen_t`. This results
in a warning like:
```
    type ‘long int’ should match type ‘flexiblas_fortran_charlen_t’
```
while linking the LAPACK fallback library. This error can safely be ignored
and is already subject to a GCC bug:

    https://gcc.gnu.org/bugzilla/show_bug.cgi?id=102079

10. NVHPC
---------
The up to at least version 24.7 of the NVHPC compiler on Aarch64, the `-z now`
flag is added to all linker calls. This avoids many parts of the hook framework
to work and even disturbs some backend detection. The problem is known to
Nvidia, see https://forums.developer.nvidia.com/t/nvc-adds-z-now-to-linking-causes-error-in-plugin-based-codes-on-arm64-gh200/307243

Furthermore, the NVHPC compiler suite leads to severval errors in the testing
suite, which also appear if one compiles the reference LAPACK with the NVHPC
compilers. Even simple routines like `dznrm2` are affected by error introduced
by the NVHPC compiler.

11. MacOS X on Apple Silicon
----------------------------
Using FlexiBLAS on MacOS X on an Apple Silicon CPU can cause problems, if gcc >=
14.0 is used. The gfortran compiler generated wrong code if at least of the
follwing flags is used: `-O2` or `-fexpensive-optimizations
-ftree-loop-vectorize`. FlexiBLAS deactivates these flags.
