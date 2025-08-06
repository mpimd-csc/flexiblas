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
#include <stdint.h>
#include "cscutils/counter.h"
#include "flexiblas_fortran_mangle.h"
/*-----------------------------------------------------------------------------
 *  Include the right header files
 *-----------------------------------------------------------------------------*/
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

#ifdef STANDALONE
int RowMajorStrg = 0;
#endif

#define RUNS 100000000

int main ( int argc, char **argv ) {

    Int N = 0;
    double DA = 1;
    Int incx = 1;
    Int incy = 1;
    Int runs = 0;
    int64_t cs, ce, csum = 0;
    unsigned his, los, hie, loe;
    csum = 0;
    for (runs = 0;  runs < RUNS; runs++) {
        __asm__ __volatile__ ("rdtsc" : "=a"(los), "=d"(his));
        FC_GLOBAL(daxpy,DAXPY)(&N, &DA, NULL, &incx, NULL, &incy);
        __asm__ __volatile__ ("rdtsc" : "=a"(loe), "=d"(hie));
        cs = (int64_t) (( (unsigned long long)los)|( ((unsigned long long)his)<<32 ));
        ce = (int64_t) (( (unsigned long long)loe)|( ((unsigned long long)hie)<<32 ));
        csum += (ce - cs);
    }
    printf("average cycles-to-quick-return daxpy: %lg\n", (double)csum/RUNS);

    csum = 0;
    for (runs = 0;  runs < RUNS; runs++) {
        __asm__ __volatile__ ("rdtsc" : "=a"(los), "=d"(his));
        FC_GLOBAL(dsyrk,DSYRK)("U","N", &N, &N, &DA, NULL, &incx, &DA, NULL, &incx, 1, 1);
        __asm__ __volatile__ ("rdtsc" : "=a"(loe), "=d"(hie));
        cs = (int64_t) (( (unsigned long long)los)|( ((unsigned long long)his)<<32 ));
        ce = (int64_t) (( (unsigned long long)loe)|( ((unsigned long long)hie)<<32 ));
        csum += (ce - cs);
    }
    printf("average cycles-to-quick-return dsyrk: %lg\n", (double)csum/RUNS);

    csum = 0;
    for (runs = 0;  runs < RUNS; runs++) {
        __asm__ __volatile__ ("rdtsc" : "=a"(los), "=d"(his));
        FC_GLOBAL(dgemv,DGEMV)("N", &N, &N, &DA, NULL, &incx, NULL, &incx, &DA, NULL, &incx, 1);
        __asm__ __volatile__ ("rdtsc" : "=a"(loe), "=d"(hie));
        cs = (int64_t) (( (unsigned long long)los)|( ((unsigned long long)his)<<32 ));
        ce = (int64_t) (( (unsigned long long)loe)|( ((unsigned long long)hie)<<32 ));
        csum += (ce - cs);
    }
    printf("average cycles-to-quick-return dgemv: %lg\n", (double)csum/RUNS);


    csum = 0;
    for (runs = 0;  runs < RUNS; runs++) {
        __asm__ __volatile__ ("rdtsc" : "=a"(los), "=d"(his));
        FC_GLOBAL(dgemm,DGEMM)("N","N", &N, &N, &N , &DA, NULL, &incx, NULL, &incy, &DA, NULL, &incx, 1, 1);
        __asm__ __volatile__ ("rdtsc" : "=a"(loe), "=d"(hie));
        cs = (int64_t) (( (unsigned long long)los)|( ((unsigned long long)his)<<32 ));
        ce = (int64_t) (( (unsigned long long)loe)|( ((unsigned long long)hie)<<32 ));
        csum += (ce - cs);
    }
    printf("average cycles-to-quick-return dgemm: %lg\n", (double)csum/RUNS);

    return 0;
}

