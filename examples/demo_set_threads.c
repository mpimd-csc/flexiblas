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


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <sys/time.h>
#include "flexiblas_fortran_mangle.h"
#include "flexiblas_api.h"

#ifndef FLEXIBLAS_CHARLEN_T
#define FLEXIBLAS_CHARLEN_T
#if __GNUC__ > 7
typedef size_t flexiblas_fortran_charlen_t;
#else
typedef int flexiblas_fortran_charlen_t;
#endif
#endif



extern void FC_GLOBAL(sgemm,SGEMM)(char* transa, char* transb, int* m, int* n, int* k, float* alpha, float* a, int* lda, float* b, int* ldb, float* beta, float* c, int* ldc, flexiblas_fortran_charlen_t len1, flexiblas_fortran_charlen_t len2);

double wtime(void)
{
    struct timeval tv;
    gettimeofday (&tv, NULL);
    return tv.tv_sec + tv.tv_usec / 1e6;
}

void gemm(int N, float *A, float *B, float *C)
{
    float fone = 1.0;
    double tic, toc;
    double flops = pow((N/1000.0),3)*2.0;

    tic = wtime();
    FC_GLOBAL(sgemm,SGEMM)("N", "N", &N, &N, &N, &fone, A, &N, B, &N , &fone, C, &N, 1, 1);
    toc = wtime();

    printf("Time: %20lg, \tGFlops = %lg \n", toc-tic, flops/(toc-tic));
    return ;
}

int main(int argc, char **argv)
{
    int N = 2000;
    FLEXIBLAS_API_INT i,j;

    float *A, *B, *C;

    if ( argc == 2 ){
        N = atoi(argv[1]);
    }
    printf("N = %d\n", N);

    A = malloc(sizeof(float) * N *N);
    B = malloc(sizeof(float) * N *N);
    C = malloc(sizeof(float) * N *N);

    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            A[j+i*N] = 1.0;
            B[j+i*N] = 1.0;
            C[j+i*N] = 0.0;
        }
    }

    printf("Using 1 Thread (C-Interface)\n");
    flexiblas_set_num_threads(1);
    gemm(N, A, B, C);

    printf("Using 2 Thread (C-Interface)\n");
    flexiblas_set_num_threads(2);
    gemm(N, A, B, C);

    printf("Using 1 Thread (F77-Interface)\n");
    i = 1;
    flexiblas_set_num_threads_(&i);
    gemm(N, A, B, C);

    printf("Using 2 Thread (F77-Interface)\n");
    i =2;
    flexiblas_set_num_threads_(&i);
    gemm(N, A, B, C);

    free(A);
    free(B);
    free(C);
    return 0;
}
