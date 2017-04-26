#include <stdlib.h>
#include <stdio.h>
// #include "f77blas_interface.h"
#include <unistd.h>
#include <sys/time.h>

#ifdef INTEGER8
	#define Int long
#else 
	#define Int int 
#endif

#define RUNS 50
void dgemm_(const char * TRANSA, const char *TRANSB, Int *m, Int *n, Int *k, double *alpha, double *A, Int *lda, double *B, Int *ldb, double *beta, double *C, Int *ldc); 

double wtime()
{
	struct timeval tv;
	gettimeofday (&tv, NULL);
	return tv.tv_sec + tv.tv_usec / 1e6;
}


int main (int argc, char **argv) {
	Int n, i; 
	double *A, *B, *C; 
	double ts,te;
	double alpha=1, beta=1;
	double flops; 
	if ( argc != 2) {
		printf("Usage: %s dim\n", argv[0]); 
		exit(1); 
	}
	n = atoi(argv[1]); 
	A = malloc(sizeof(double) * n *n ); 
	B = malloc(sizeof(double) * n *n ); 
	C = malloc(sizeof(double) * n *n ); 

	for ( i = 0; i < n * n; i++){
		A[i]=i+1; 
		B[i]=i+0.5; 
	}

	/*-----------------------------------------------------------------------------
	 *  Warmup 
	 *-----------------------------------------------------------------------------*/
	dgemm_("N","N", &n,&n,&n,&alpha, A, &n, B,&n, &beta, C, &n); 
	dgemm_("N","N", &n,&n,&n,&alpha, A, &n, B,&n, &beta, C, &n); 
	dgemm_("N","N", &n,&n,&n,&alpha, A, &n, B,&n, &beta, C, &n); 

	ts = wtime(); 
	for (i=0; i < RUNS; i++){
		dgemm_("N","N", &n,&n,&n,&alpha, A, &n, B,&n, &beta, C, &n); 
	}
	te = wtime(); 
	double h = (double) n / 1000.0; 
	flops = 2.0 * h *h *h;
	flops /= ((te-ts)/RUNS); 
	printf("time: %lg\n", (te-ts)/RUNS);
	printf("flops: %lg GFlop/s\n", flops );

	free(A); 
	free(B);
	free(C); 
	return 0; 
}

