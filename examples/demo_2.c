//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2025 Martin Koehler

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
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



double wtime(void)
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
        FC_GLOBAL(dgemm,DGEMM)("N","N", &n,&n,&n,&alpha, A, &n, B,&n, &beta, C, &n, 1, 1);
    }
    te = wtime();
    printf("time: %lg\n", (te-ts)/10.0);


    free(A);
    free(B);
    free(C);
    return 0;
}
