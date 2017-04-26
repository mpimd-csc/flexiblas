Issues using FlexiBLAS 
======================

Last Update: March 29, 2017

Table of Contents 
-----------------

1. Profiling Numpy/Scipy with linked against FlexiBLAS 
2. CBLAS Tests with ATLAS and MKL
3. Building software with FlexiBLAS
4. FlexiBLAS-API is not linked. 
5. Crash with LAPACK interface on IBM Power 


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
