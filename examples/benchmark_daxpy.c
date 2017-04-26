#include <stdlib.h>
#include <stdio.h>
// #include "f77blas_interface.h"
#include <unistd.h>
#include <sys/time.h>
#include "cblas.h"

#define RUNS 20000

#ifdef INTEGER8
	#define Int long
#else 
	#define Int int 
#endif


void daxpy_(Int *N, double *alpha, double *x, Int *incx, double *Y, Int *incy); 


double wtime()
{
	struct timeval tv;
	gettimeofday (&tv, NULL);
	return tv.tv_sec + tv.tv_usec / 1e6;
}


int main (int argc, char **argv) {
	Int n, i; 
	double *A, *B; 
	double ts,te;
	double alpha=1;
	double flops;
	Int incx = 1, incy = 1; 
	if ( argc != 2) {
		printf("Usage: %s dim\n", argv[0]); 
		exit(1); 
	}
	n = atoi(argv[1]); 
	A = malloc(sizeof(double) * n ); 
	B = malloc(sizeof(double) * n ); 

	for ( i = 0; i < n ; i++){
		A[i]=i+1; 
		B[i]=i+0.5; 
	}
	printf("F77_BLAS\n");
	ts = wtime(); 
	for (i=0; i < RUNS; i++){
		daxpy_(&n,&alpha, A, &incx, B, &incy);  
	}
	te = wtime(); 
	flops = 2.0 * n;
	flops /=1000*1000*1000; 
	flops /= (te-ts)/RUNS; 
	printf("time: %lg\n", (te-ts)/RUNS);
	printf("flops: %lg GFlop/s\n", flops );
#ifdef FLEXIBLAS_CBLAS 
	printf("\nC_BLAS\n");
	ts = wtime(); 
	for (i=0; i < RUNS; i++){
		cblas_daxpy(n,alpha, A, incx, B, incy);  
	}
	te = wtime(); 
	flops = 2.0 * n;
	flops /=1000*1000*1000; 
	flops /= (te-ts)/RUNS; 
	printf("time: %lg\n", (te-ts)/RUNS);
	printf("flops: %lg GFlop/s\n", flops );
#endif 
	free(A); 
	free(B);
	return 0; 
}

