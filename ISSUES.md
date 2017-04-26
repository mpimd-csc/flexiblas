Issues using FlexiBLAS 
======================

Last Update: Oct 14, 2015

Table of Contents 
-----------------

1. Profiling Numpy/Scipy with linked against FlexiBLAS 
2. CBLAS Tests with ATLAS and MKL
3. Building software with FlexiBLAS


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


