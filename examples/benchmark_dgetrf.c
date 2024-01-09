//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program. If not, see <https://www.gnu.org/licenses/>.
 */





#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/time.h>
#include "flexiblas_fortran_mangle.h"

#ifdef INTEGER8
	#define Int long
#else
	#define Int int
#endif

#define RUNS 1

void FC_GLOBAL(dgetrf,DGETRF)(Int *n , Int *m, double *A, Int *lda, Int *ipiv, Int *info);
double wtime(void)
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
		FC_GLOBAL(dgetrf,DGETRF)(&n,&n, B, &n, ipiv, &info);
		memcpy(B,A,sizeof(double)*n*n);
		FC_GLOBAL(dgetrf,DGETRF)(&n,&n, B, &n, ipiv, &info);

	ts = wtime();
	for (i=0; i < RUNS; i++){
		memcpy(B,A,sizeof(double)*n*n);
		FC_GLOBAL(dgetrf,DGETRF)(&n,&n, B, &n, ipiv, &info);
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

