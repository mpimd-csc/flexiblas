# Checks if a given BLAS library contains the IntelMKL calling sequence 
# for ZDOTC. If this holds true ZDOTC_MKL is set to true 
# 
#
#

INCLUDE(CheckCSourceRuns)

MACRO(CheckBlasZdotcMKL BLAS_LIBS BLAS_LD_FLAGS INT8)
	SET( __CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS})
	SET( __CMAKE_REQUIRED_DEFINITIONS ${CMAKE_REQUIRED_DEFINITIONS})
	SET( __CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES})
	SET( __CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES})
	UNSET(ZDOTC_MKL CACHE)
	IF ( INT8 ) 
		SET(CMAKE_REQUIRED_DEFINITIONS "-DInt=int64_t")
	ELSE (INT8) 
		SET(CMAKE_REQUIRED_DEFINITIONS "-DInt=int")
	ENDIF(INT8) 
	SET(CMAKE_REQUIRED_LIBRARIES ${BLAS_LIBS})
	SET(CMAKE_REQUIRED_FLAGS ${BLAS_LD_FLAGS}) 
	Check_C_SOURCE_RUNS(
			"
#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <stdint.h>
double complex zdotc_(Int *n, double complex *X, Int *incx, double complex *Y, Int *INCY , Int *dummy); 
int main ( ) {
Int n = 4; 
Int incx = 1, incy = 1; 
Int dummy= 1; 
double complex x[4] = {1,1,3,4}; 
double complex y[4] = {2,2,1,1}; 
double complex ret; 
ret = zdotc_(&n,x,&incx,y,&incy,&dummy); 
printf(\"n = %d ret = %lg\", n, creal(ret)); 
if (n!=4) return 0; 
return -1; 
	abort(); 
}"
	ZDOTC_MKL)
	SET( CMAKE_REQUIRED_FLAGS ${__CMAKE_REQUIRED_FLAGS})
	SET( CMAKE_REQUIRED_DEFINITIONS ${__CMAKE_REQUIRED_DEFINITIONS})
	SET( CMAKE_REQUIRED_INCLUDES ${__CMAKE_REQUIRED_INCLUDES})
	SET( CMAKE_REQUIRED_LIBRARIES ${__CMAKE_REQUIRED_LIBRARIES})

ENDMACRO(CheckBlasZdotcMKL) 


