#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>

#ifdef INTEGER8
	#define Int long
#else 
	#define Int int 
#endif

#define RUNS 2000
void dgemv_(const char * TRANSA, Int *m, Int *n, double *alpha, double *A, Int *lda, double *B, Int *incb, double *beta, double *C, Int *incc); 

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
	Int incb = 1, incc = 1; 
	if ( argc != 2) {
		printf("Usage: %s dim\n", argv[0]); 
		exit(1); 
	}
	n = atoi(argv[1]); 
	A = malloc(sizeof(double) * n *n ); 
	B = malloc(sizeof(double) * n ); 
	C = malloc(sizeof(double) * n ); 

	for ( i = 0; i < n * n; i++){
		A[i]=i+1; 
	}
	for (i = 0; i < n; i++) {
		B[i]=i*2+1; 
		C[i]=1; 
	}
	ts = wtime(); 
	for (i=0; i < RUNS; i++){
		dgemv_("N", &n,&n,&alpha, A, &n, B,&incb, &beta, C, &incc); 
	}
	te = wtime(); 
	flops = 2.0 * n *n;
	flops /=1000*1000*1000;
	flops /= (te-ts)/RUNS; 
	printf("time: %lg\n", (te-ts)/RUNS);
	printf("flops: %lg GFlop/s\n", flops );

	free(A); 
	free(B);
	free(C); 
	return 0; 
}

