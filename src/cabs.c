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



#include "flexiblas_fortran_mangle.h"



#include <stdio.h>
#include <math.h>
#include <complex.h>
#include "flexiblas.h"

double FC_GLOBAL(dcabs1,DCABS1)(const double complex *z)
{
    return fabs(creal(*z)) + fabs(cimag(*z));
}

float FC_GLOBAL(scabs1,SCABS1)(const float complex *z)
{
    return fabsf(crealf(*z)) + fabsf(cimagf(*z));
}

double FC_GLOBAL2(dcabs1,DCABS1)(const double complex *z)
{
    return fabs(creal(*z)) + fabs(cimag(*z));
}

float FC_GLOBAL2(scabs1,SCABS1)(const float complex *z)
{
    return fabsf(crealf(*z)) + fabsf(cimagf(*z));
}

double FC_GLOBAL3(dcabs1,DCABS1)(const double complex *z)
{
    return fabs(creal(*z)) + fabs(cimag(*z));
}

float FC_GLOBAL3(scabs1,SCABS1)(const float complex *z)
{
    return fabsf(crealf(*z)) + fabsf(cimagf(*z));
}


#ifdef FLEXIBLAS_CBLAS
double cblas_dcabs1(const void *c)
{
    return FC_GLOBAL(dcabs1,DCABS1)((const double complex *) c);
}

double cblas_scabs1(const void *c)
{
    return FC_GLOBAL(scabs1,SCABS1)((const float complex *) c);
}

#endif
