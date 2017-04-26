#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>

#ifdef INTEGER8
	#define Int long
#else 
	#define Int int 
#endif

#define RUNS 50

void dgetrf_(Int *n , Int *m, double *A, Int *lda, Int *ipiv, Int *info); 
double wtime()
{
	struct timeval tv;
	gettimeofday (&tv, NULL);
	return tv.tv_sec + tv.tv_usec / 1e6;
}


int main (int argc, char **argv) {
	Int n, i,j; 
	double *A;
	double *B; 
	Int *ipiv; 
	Int info; 
	double ts,te;
	double flops; 
	if ( argc != 2) {
		printf("Usage: %s dim\n", argv[0]); 
		exit(1); 
	}
	n = atoi(argv[1]); 
	A = malloc(sizeof(double) * n *n ); 
	B = malloc(sizeof(double) *n*n); 

	ipiv = malloc(sizeof(Int) * n ); 
	for (i = 0; i < n; i++) {
		for (j = 0; j < n; j++) {
			A[i+j*n] = 1.0/(i+j+1); 
		}
	}
		memcpy(B,A,sizeof(double)*n*n); 
		dgetrf_(&n,&n, B, &n, ipiv, &info); 
		memcpy(B,A,sizeof(double)*n*n); 
		dgetrf_(&n,&n, B, &n, ipiv, &info); 

	ts = wtime(); 
	for (i=0; i < RUNS; i++){
		memcpy(B,A,sizeof(double)*n*n); 
		dgetrf_(&n,&n, B, &n, ipiv, &info); 
	}
	te = wtime(); 
	double h = (double) n / 1000.0; 
	flops = 2.0/3.0 * h *h *h;
	flops /= (te-ts)/RUNS; 
	printf("time: %lg\n", (te-ts)/RUNS);
	printf("flops: %lg GFlop/s\n", flops );

	free(A); 
	free(ipiv); 
	return 0; 
}

