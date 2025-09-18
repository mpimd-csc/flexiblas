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
#include "lapacke.h"

/** void dgesv_(int *n, int *nrhs, double *A, int *lda , int *ipiv, double *b, int *ldb, int *info); */

int main(int argc, char ** argv) {
    (void) argc;
    (void) argv;

    int n = 4;
    int nrhs = 1;
    int lda = 4;
    int ldb = 4;

    double A[4*4] = {
        3.0, 2.0, 3.0, -4.0,
        -5.0, 6.0, -7.0, 8.0,
        9.0, 10.0, 11.0, -12.0,
        -13.0, 14.0, -15.0, 16.0
    };

    double b[4] = {1.0, 2.0, 3.0, 4.0};

    lapack_int ipiv[4] = {0, 0, 0, 0};

    int info = LAPACKE_dgesv(LAPACK_COL_MAJOR, n, nrhs, A, lda, ipiv, b, ldb);
    /** int info = 0; */
    /** dgesv_(&n, &nrhs, A, &lda, ipiv, b, &ldb, &info); */

    if (info == 0) {
        printf("Solution is :\n");
        for (int i = 0; i < n; i++) {
            printf("%lf\n", b[i]);
        }
    } else {
        printf("Failed to solve Ax=b. Info = %d\n", info);
    }

    return 0;
}
