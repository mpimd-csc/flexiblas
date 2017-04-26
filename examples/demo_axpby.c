/* $Id$ */
/**
 * @file demo_axpby.c
 * @brief Demonstrate AXPBY  
 * @author Martin Köhler
 * @version $Id$ 
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>

#ifdef INTEGER8
#define Int int64_t
#define USE_BLAS_64
#else 
#define Int int
#endif 

#ifdef BLAS_INTERFACE_INTEL 
#include "blas_intel.h"
#include "extblas_intel.h"
#else 
#include "blas_gnu.h"
#include "extblas_gnu.h"
#endif 
#ifdef FLEXIBLAS_CBLAS
#include "cblas.h" 
#endif 







int main(int argc, const char *argv[])
{
	{
		Int n = 4, incx = 1, incy = 1; 
		int i; 
		double dx[4] = {1, 1, 1, 1}; 
		double dy[4] = {2, 2, 2, 2}; 
		double da = 2; 
		double db = 1.5; 

		daxpby_(&n, &da, dx, &incx, &db, dy, &incy); 
		printf("DAXPBY:\n");
		for (i = 0; i < n; i++) {
			printf("[%2d] = %lg\n", i, dy[i]);
		}
	}
	{
		Int n = 4, incx = 1, incy = 1; 
		int i; 
		float dx[4] = {1, 1, 1, 1}; 
		float dy[4] = {2, 2, 2, 2}; 
		float da = 2; 
		float db = 1.5; 

		saxpby_(&n, &da, dx, &incx, &db, dy, &incy); 
		printf("SAXPBY:\n");
		for (i = 0; i < n; i++) {
			printf("[%2d] = %lg\n", i, dy[i]);
		}
	}

	{
		Int n = 4, incx = 1, incy = 1;
		int i; 
		double complex dx[4] = {1, 1, 1, 1}; 
		double complex dy[4] = {2 + I, 2-I, 2+I, 2-I}; 
		double complex da = 2; 
		double complex db = 1.5; 

		zaxpby_(&n, &da, dx, &incx, &db, dy, &incy); 
		printf("ZAXPBY:\n");
		for (i = 0; i < n; i++) {
			printf("[%2d] = %lg + %lg\n", i, creal(dy[i]), cimag(dy[i]));
		}
	}

	{
		Int n = 4, incx = 1, incy = 1; 
		int i; 
		float complex dx[4] = {1, 1, 1, 1}; 
		float complex dy[4] = {2 + I, 2-I, 2+I, 2-I}; 
		float complex da = 2; 
		float complex db = 1.5; 

		caxpby_(&n, &da, dx, &incx, &db, dy, &incy); 
		printf("CAXPBY:\n");
		for (i = 0; i < n; i++) {
			printf("[%2d] = %lg + %lg\n", i, creal(dy[i]), cimag(dy[i]));
		}
	}
#ifdef FLEXIBLAS_CBLAS
	printf("CBLAS Tests:\n");
	{
		int n = 4, incx = 1, incy = 1, i; 
		double dx[4] = {1, 1, 1, 1}; 
		double dy[4] = {2, 2, 2, 2}; 
		double da = 2; 
		double db = 1.5; 

		cblas_daxpby(n, da, dx, incx, db, dy, incy); 
		printf("CBLAS_DAXPBY:\n");
		for (i = 0; i < n; i++) {
			printf("[%2d] = %lg\n", i, dy[i]);
		}
	}
	{
		int n = 4, incx = 1, incy = 1, i; 
		float dx[4] = {1, 1, 1, 1}; 
		float dy[4] = {2, 2, 2, 2}; 
		float da = 2; 
		float db = 1.5; 

		cblas_saxpby(n, da, dx, incx, db, dy, incy); 
		printf("CBLAS_SAXPBY:\n");
		for (i = 0; i < n; i++) {
			printf("[%2d] = %lg\n", i, dy[i]);
		}
	}

	{
		int n = 4, incx = 1, incy = 1, i; 
		double complex dx[4] = {1, 1, 1, 1}; 
		double complex dy[4] = {2 + I, 2-I, 2+I, 2-I}; 
		double complex da = 2; 
		double complex db = 1.5; 

		cblas_zaxpby(n, &da, dx, incx, &db, dy, incy); 
		printf("CBLAS_ZAXPBY:\n");
		for (i = 0; i < n; i++) {
			printf("[%2d] = %lg + %lg\n", i, creal(dy[i]), cimag(dy[i]));
		}
	}

	{
		int n = 4, incx = 1, incy = 1, i; 
		float complex dx[4] = {1, 1, 1, 1}; 
		float complex dy[4] = {2 + I, 2-I, 2+I, 2-I}; 
		float complex da = 2; 
		float complex db = 1.5; 

		cblas_caxpby(n, &da, dx, incx, &db, dy, incy); 
		printf("CBLAS_CAXPBY:\n");
		for (i = 0; i < n; i++) {
			printf("[%2d] = %lg + %lg\n", i, creal(dy[i]), cimag(dy[i]));
		}
	}



#endif

	return 0;
}
