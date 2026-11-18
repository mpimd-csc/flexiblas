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



#include <stdlib.h>
#include <stdio.h>
#include "cblas.h"
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

int main ( int argc, char **argv ) {
    printf("zdotc gnu\n");
    blas_complex_double a[]={flexiblas_make_complex_double(1,1),flexiblas_make_complex_double(2,1),flexiblas_make_complex_double(3,0),flexiblas_make_complex_double(4,0)};
    blas_complex_double b[]={flexiblas_make_complex_double(1,0),flexiblas_make_complex_double(1,0),flexiblas_make_complex_double(1,0),flexiblas_make_complex_double(1,0)};
    Int n = 4;
    Int one = 1;
    blas_complex_double ret = flexiblas_make_complex_double(0,0), ret2 = ret;

#ifdef FLEXIBLAS_CBLAS
    n = 4;
    one = 1;
    cblas_zdotc_sub(n, a, one,b,one,&ret2);
    printf("cblas_zdotc(test) = %lg + %lg\n", lapack_complex_double_real(ret2), lapack_complex_double_imag(ret2) );
#endif

    ret = FC_GLOBAL(zdotc,ZDOTC)(&n, a, &one,b, &one);
    printf("zdotc_(test)      = %lg  + %lg\n", lapack_complex_double_real(ret), lapack_complex_double_imag(ret) );

    return 0;
}
