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

#include "flexiblas_fortran_mangle.h"

#include <stdio.h>
#include <math.h>
#include "flexiblas.h"

double FC_GLOBAL(dcabs1,DCABS1)(const lapack_complex_double *z)
{
    return fabs(lapack_complex_double_real(*z)) + fabs(lapack_complex_double_imag(*z));
}

float FC_GLOBAL(scabs1,SCABS1)(const lapack_complex_float *z)
{
    return fabsf(lapack_complex_float_real(*z)) + fabsf(lapack_complex_float_imag(*z));
}

double FC_GLOBAL2(dcabs1,DCABS1)(const lapack_complex_double *z)
{
    return fabs(lapack_complex_double_real(*z)) + fabs(lapack_complex_double_imag(*z));
}

float FC_GLOBAL2(scabs1,SCABS1)(const lapack_complex_float *z)
{
    return fabsf(lapack_complex_float_real(*z)) + fabsf(lapack_complex_float_imag(*z));
}

double FC_GLOBAL3(dcabs1,DCABS1)(const lapack_complex_double *z)
{
    return fabs(lapack_complex_double_real(*z)) + fabs(lapack_complex_double_imag(*z));
}

float FC_GLOBAL3(scabs1,SCABS1)(const lapack_complex_float *z)
{
    return fabsf(lapack_complex_float_real(*z)) + fabsf(lapack_complex_float_imag(*z));
}


#ifdef FLEXIBLAS_CBLAS
double cblas_dcabs1(const void *c)
{
    return FC_GLOBAL(dcabs1,DCABS1)((const lapack_complex_double *) c);
}

double cblas_scabs1(const void *c)
{
    return FC_GLOBAL(scabs1,SCABS1)((const lapack_complex_float *) c);
}

#endif
