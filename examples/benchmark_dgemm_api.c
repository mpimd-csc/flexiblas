#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>

#include "flexiblas_api.h"

#ifdef INTEGER8
	#define Int long
#else 
	#define Int int 
#endif

void dgemm_(const char * TRANSA, const char *TRANSB, Int *m, Int *n, Int *k, double *alpha, double *A, Int *lda, double *B, Int *ldb, double *beta, double *C, Int *ldc); 

double wtime()
{
	struct timeval tv;
	gettimeofday (&tv, NULL);
	return tv.tv_sec + tv.tv_usec / 1e6;
}


int main (int argc, char **argv) {
	Int n; 
	double *A, *B, *C; 
	double ts,te;
	double alpha=1, beta=1;
	double flops; 
    Int RUNS; 
    int major, minor, patch; 
    char fb_name[128]; 
    int ids[1024]; 
    int i, b; 

	if ( argc != 3) {
		printf("Usage: %s dim runs\n", argv[0]); 
		exit(1); 
	}

    /*-----------------------------------------------------------------------------
     *  Startup
     *-----------------------------------------------------------------------------*/
    flexiblas_get_version(&major, &minor, &patch); 
    printf("FlexiBLAS Version %d.%d.%d\n\n", major, minor, patch);
    printf("Available Backends:\n");
    flexiblas_print_avail_backends(stdout); 
    printf("\n");
    int nbackends = flexiblas_list(NULL, 0 , 0); 
    for (i = 0; i < nbackends; i++) {
        flexiblas_list(fb_name, 128, i); 
        printf("Load %s.\n", fb_name);
        ids[i] = flexiblas_load_backend(fb_name); 
    }


	n = atoi(argv[1]); 
    RUNS = atoi(argv[2]); 
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

    for (b = 2; b < nbackends-1; b++) {
        flexiblas_switch(ids[b]); 

        flexiblas_current_backend(fb_name, 128);
         // printf("%s\n", fb_name);
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
        printf("%25s \t time: %lg\t ", fb_name, (te-ts)/RUNS);
        printf("flops: %lg GFlop/s\n", flops );

    }

	free(A); 
	free(B);
	free(C); 
	return 0; 
}

