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
#include <complex.h>
#include "flexiblas_fortran_mangle.h"
#include "flexiblas_config.h"
#ifndef Int
#ifndef FLEXIBLAS_INTEGER8
#define Int 	int
#else
#include <stdint.h>
#define Int 	int64_t
#endif
#endif

#include "cblas.h"

extern void FC_GLOBAL(zdotc,ZDOTC)( double complex* RETURNVALUE, Int* N, double complex* ZX, Int* INCX, double complex* ZY, Int* INCY);

int main ( int argc, char **argv ) {
    printf("zdotc gnu\n");
    double complex a[]={1+I,2+I,3,4};
    double complex b[]={1,1,1,1};
    Int n = 4;
    Int one = 1;
    double complex ret = 0, ret2 = 0;
    FC_GLOBAL(zdotc,ZDOTC)(&ret, &n, a, &one,b, &one);
    printf("zdotc_(test)      = %lg  + %lg\n", creal(ret), cimag(ret) );

#ifdef FLEXIBLAS_CBLAS
    n = 4;
    one = 1;
    cblas_zdotc_sub(n, a, one,b,one,&ret2);
    printf("cblas_zdotc(test) = %lg + %lg\n", creal(ret2), cimag(ret2) );
#endif
    return 0;
}

