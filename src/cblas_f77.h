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


#ifndef CBLAS_F77_H
#define CBLAS_F77_H
#include <stdlib.h>
#include <complex.h>
#include "flexiblas_fortran_mangle.h"
#include "flexiblas.h"
#include "flexiblas_config.h"
#include "flexiblas_fortran_char_len.h"

#ifdef FLEXIBLAS_INTEGER8
#include <stdint.h>
#define F77_INT int64_t
#endif

#ifdef  F77_CHAR
#define FCHAR F77_CHAR
#else
#define FCHAR char *
#endif

#ifdef F77_INT
#define FINT const F77_INT *
#define FINT2 F77_INT *
#else
#define FINT const int *
#define FINT2 int *
#endif

#define COPY_CONST_PTR(a,b) memcpy(&a, &b, sizeof(void*))
#ifdef FLEXIBLAS_ABI_INTEL
#include "blas_intel.h"
#else
#include "blas_gnu.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

    extern void FC_GLOBAL(xerbla,XERBLA)(FCHAR, void *, flexiblas_fortran_charlen_t);


#ifdef __cplusplus
}
#endif

#endif /*  CBLAS_F77_H */
