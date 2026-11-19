//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2026 Martin Koehler

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
#include <math.h>
#include "flexiblas_fortran_mangle.h"

#ifdef INTEGER8
#define Int int64_t
#define USE_BLAS_64
#else
#define Int int
#endif

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







int main(int argc, const char *argv[])
{
    {
        Int n = 4, incx = 1, incy = 1;
        int i;
        double dx[4] = {1, 2, 3, 4};
        double dy[4] = {5, 6, 7, 8};
        double da = 2;
        double db = 1.5;

        FC_GLOBAL(daxpby,DAXPBY)(&n, &da, dx, &incx, &db, dy, &incy);
        printf("DAXPBY:\n");
        for (i = 0; i < n; i++) {
            printf("[%2d] = %lg\n", i, dy[i]);
        }
    }
    {
        Int n = 4, incx = 1, incy = 1;
        int i;
        float dx[4] = {1, 2, 3, 4};
        float dy[4] = {5, 6, 7, 8};
        float da = 2;
        float db = 1.5;

        FC_GLOBAL(saxpby,SAXPBY)(&n, &da, dx, &incx, &db, dy, &incy);
        printf("SAXPBY:\n");
        for (i = 0; i < n; i++) {
            printf("[%2d] = %lg\n", i, dy[i]);
        }
    }

    {
        Int n = 4, incx = 1, incy = 1;
        int i;
        blas_complex_double dx[4] = {flexiblas_make_complex_double(1,0), flexiblas_make_complex_double(1,0), flexiblas_make_complex_double(1,0), flexiblas_make_complex_double(1,0)};
        blas_complex_double dy[4] = {flexiblas_make_complex_double(2,1), flexiblas_make_complex_double(2,-1), flexiblas_make_complex_double(2,1), flexiblas_make_complex_double(2,-1)};
        blas_complex_double da = flexiblas_make_complex_double(2,0);
        blas_complex_double db = flexiblas_make_complex_double(1.5,0);

        FC_GLOBAL(zaxpby,ZAXPBY)(&n, &da, dx, &incx, &db, dy, &incy);
        printf("ZAXPBY:\n");
        for (i = 0; i < n; i++) {
            printf("[%2d] = %lg + %lg\n", i, lapack_complex_double_real(dy[i]), lapack_complex_double_imag(dy[i]));
        }
    }

    {
        Int n = 4, incx = 1, incy = 1;
        int i;
        blas_complex_float dx[4] = {flexiblas_make_complex_float(1,0), flexiblas_make_complex_float(1,0), flexiblas_make_complex_float(1,0), flexiblas_make_complex_float(1,0)};
        blas_complex_float dy[4] = {flexiblas_make_complex_float(2,1), flexiblas_make_complex_float(2,-1), flexiblas_make_complex_float(2,1), flexiblas_make_complex_float(2,-1)};
        blas_complex_float da = flexiblas_make_complex_float(2,0);
        blas_complex_float db = flexiblas_make_complex_float(1.5,0);

        FC_GLOBAL(caxpby,CAXPBY)(&n, &da, dx, &incx, &db, dy, &incy);
        printf("CAXPBY:\n");
        for (i = 0; i < n; i++) {
            printf("[%2d] = %lg + %lg\n", i, lapack_complex_float_real(dy[i]), lapack_complex_float_imag(dy[i]));
        }
    }
#ifdef FLEXIBLAS_CBLAS
    printf("CBLAS Tests:\n");
    {
        int n = 4, incx = 1, incy = 1, i;
        double dx[4] = {1, 1, 1, 1};
        double dy[4] = {2, 2, 2, 2};
        double da = 2;
        double db = 1.5;

        cblas_daxpby(n, da, dx, incx, db, dy, incy);
        printf("CBLAS_DAXPBY:\n");
        for (i = 0; i < n; i++) {
            printf("[%2d] = %lg\n", i, dy[i]);
        }
    }
    {
        int n = 4, incx = 1, incy = 1, i;
        float dx[4] = {1, 1, 1, 1};
        float dy[4] = {2, 2, 2, 2};
        float da = 2;
        float db = 1.5;

        cblas_saxpby(n, da, dx, incx, db, dy, incy);
        printf("CBLAS_SAXPBY:\n");
        for (i = 0; i < n; i++) {
            printf("[%2d] = %lg\n", i, dy[i]);
        }
    }

    {
        int n = 4, incx = 1, incy = 1, i;
        blas_complex_double dx[4] = {flexiblas_make_complex_double(1,0), flexiblas_make_complex_double(1,0), flexiblas_make_complex_double(1,0), flexiblas_make_complex_double(1,0)};
        blas_complex_double dy[4] = {flexiblas_make_complex_double(2,1), flexiblas_make_complex_double(2,-1), flexiblas_make_complex_double(2,1), flexiblas_make_complex_double(2,-1)};
        blas_complex_double da = flexiblas_make_complex_double(2,0);
        blas_complex_double db = flexiblas_make_complex_double(1.5,0);

        cblas_zaxpby(n, &da, dx, incx, &db, dy, incy);
        printf("CBLAS_ZAXPBY:\n");
        for (i = 0; i < n; i++) {
            printf("[%2d] = %lg + %lg\n", i, lapack_complex_double_real(dy[i]), lapack_complex_double_imag(dy[i]));
        }
    }

    {
        int n = 4, incx = 1, incy = 1, i;
        blas_complex_float dx[4] = {flexiblas_make_complex_float(1,0), flexiblas_make_complex_float(1,0), flexiblas_make_complex_float(1,0), flexiblas_make_complex_float(1,0)};
        blas_complex_float dy[4] = {flexiblas_make_complex_float(2,1), flexiblas_make_complex_float(2,-1), flexiblas_make_complex_float(2,1), flexiblas_make_complex_float(2,-1)};
        blas_complex_float da = flexiblas_make_complex_float(2,0);
        blas_complex_float db = flexiblas_make_complex_float(1.5,0);

        cblas_caxpby(n, &da, dx, incx, &db, dy, incy);
        printf("CBLAS_CAXPBY:\n");
        for (i = 0; i < n; i++) {
            printf("[%2d] = %lg + %lg\n", i, lapack_complex_float_real(dy[i]), lapack_complex_float_imag(dy[i]));
        }
    }



#endif

    return 0;
}
