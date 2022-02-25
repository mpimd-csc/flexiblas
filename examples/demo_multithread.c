/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2016
 */





#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>

#include "flexiblas_fortran_mangle.h"

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

#ifdef INTEGER8
#define Int int64_t
#else
#define Int int
#endif

#include<omp.h>

double wtime()
{
	struct timeval tv;
	gettimeofday (&tv, NULL);
	return tv.tv_sec + tv.tv_usec / 1e6;
}


int main (int argc, char **argv) {
	Int n, i, j;
	double **A, **B, **C;
	double ts,te;
	double alpha=1, beta=1;
    int nthread = omp_get_max_threads();
	if ( argc != 2) {
		printf("Usage: %s dim\n", argv[0]);
		exit(1);
	}
	n = atoi(argv[1]);
    printf("Number of threads = %d\n", nthread);
    A = (double**) malloc(sizeof(double*) * (nthread));
    B = (double**) malloc(sizeof(double*) * (nthread));
    C = (double**) malloc(sizeof(double*) * (nthread));

    for (j = 0; j < nthread; j++) {
    	A[j] = malloc(sizeof(double) * n *n );
    	B[j] = malloc(sizeof(double) * n *n );
    	C[j] = malloc(sizeof(double) * n *n );
    	for ( i = 0; i < n * n; i++){
	    	A[j][i]=i+1;
    		B[j][i]=i+0.5;
	    }
    }


	ts = wtime();
    #pragma omp parallel private(i)
    {
        int id = omp_get_thread_num();
    	for (i=0; i < 400; i++){
	    	FC_GLOBAL(dgemm,DGEMM)("N","N", &n,&n,&n,&alpha, A[id], &n, B[id],&n, &beta, C[id], &n);
    	}
    }

	te = wtime();
	printf("time: %lg\n", (te-ts)/10.0);

    for (i = 0; i < nthread; i++) {
        free(A[i]);
        free(B[i]);
        free(C[i]);

    }
    free(A);
    free(B);
    free(C);

	return 0;
}
