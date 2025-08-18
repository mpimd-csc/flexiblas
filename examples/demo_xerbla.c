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



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "flexiblas_fortran_mangle.h"
#ifndef Int
#ifndef INTEGER8
#define Int 	int
#else
#include <stdint.h>
#define Int 	int64_t
#endif
#endif

#ifndef FLEXIBLAS_CHARLEN_T
#define FLEXIBLAS_CHARLEN_T
#if __GNUC__ > 7
typedef size_t flexiblas_fortran_charlen_t;
#else
typedef int flexiblas_fortran_charlen_t;
#endif
#endif


void FC_GLOBAL(dgemm,DGEMM)(char *, char *, Int *, Int *, Int *, double *, double *, Int *, double *, Int *, double *, double *, Int *, flexiblas_fortran_charlen_t len1, flexiblas_fortran_charlen_t len2);


void xerbla_(char *name, Int *info, Int len){
    char * ptr = malloc ( sizeof(char) * (len + 1));
    strncpy(ptr, name, len);
    ptr[len] = '\0';
    printf("name: %s\n", ptr);
    printf("info: %d\n", (int) *info);
    printf("len:  %d\n", (int) len);
    free(ptr);
}

int main(int argc, const char *argv[])
{
    Int n,m,k;
    Int lda, ldb, ldc;
    double alpha, beta;
    double A[10],B[10],C[10];

    m = 2;
    n = 2;
    k = 2;
    lda = 1;
    ldb = 2;
    ldc = 0;
    alpha = beta = 1;
    // printf("xerbla_: %lx\n", (unsigned long)((void*)&xerbla_));
    FC_GLOBAL(dgemm,DGEMM)("N", "N", &m, &n, &k, &alpha, A, &lda, B, &ldb, &beta, C, &ldc, 1, 1);


    return 0;
}
