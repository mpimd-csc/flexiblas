#include <stdlib.h>
#include <stdio.h>
#include "f77blas_interface.h"
#include <unistd.h>
#include <sys/time.h>


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
	ts = wtime(); 
	for (i=0; i < 10; i++){
		dgemm_("N","N", &n,&n,&n,&alpha, A, &n, B,&n, &beta, C, &n); 
	}
	te = wtime(); 
	printf("time: %lg\n", (te-ts)/10.0);


	free(A); 
	free(B);
	free(C); 
	return 0; 
}
