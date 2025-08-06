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






#include "cblas.h"
#include "cblas_f77.h"
#include "cblas_f77_ext.h"

void cblas_cgeadd(const CBLAS_ORDER CORDER,
        const CBLAS_INT crows, const CBLAS_INT ccols, const void *alpha, void *ca, const CBLAS_INT clda,
        const void *beta, void *cb, const CBLAS_INT cldb)
{

#ifdef F77_INT
    F77_INT F77_LDA =clda;
    F77_INT F77_LDB =cldb;
#else
#define F77_LDA  clda
#define F77_LDB  cldb
#endif

#ifdef F77_INT
    F77_INT t = 0;
    F77_INT rows = crows;
    F77_INT cols = ccols;
#else
    CBLAS_INT t = 0;
    CBLAS_INT rows = crows;
    CBLAS_INT cols = ccols;
#endif

    if ( CORDER == CblasRowMajor ) {
        t = rows;
        rows = cols;
        cols = t;
    }
    F77_cgeadd( &rows, &cols, alpha, ca,  &F77_LDA, beta, cb, &F77_LDB);
}

