//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2024 Martin Koehler

   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with this program. If not, see <https://www.gnu.org/licenses/>.
   */

#ifndef EXTBLAS_H
#define EXTBLAS_H

#include <stdint.h>

#include "flexiblas_fortran_mangle.h"
#include "flexiblas_fortran_char_len.h"

#include <complex.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "flexiblas_config.h"


    void FC_GLOBAL(caxpby,CAXPBY)(blasint* n, float complex* ca, float complex* cx, blasint* incx, float complex* cb, float complex* cy, blasint* incy);

    void FC_GLOBAL(daxpby,DAXPBY)(blasint* n, double* da, double* dx, blasint* incx, double* db, double* dy, blasint* incy);

    void FC_GLOBAL(zaxpby,ZAXPBY)(blasint* n, double complex* za, double complex* zx, blasint* incx, double complex* zb, double complex* zy, blasint* incy);

    void FC_GLOBAL(saxpby,SAXPBY)(blasint* n, float* sa, float* sx, blasint* incx, float* sb, float* sy, blasint* incy);

    void FC_GLOBAL(comatcopy,COMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_order, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zomatcopy,ZOMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_order, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(domatcopy,DOMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, double* b, blasint* ldb, flexiblas_fortran_charlen_t len_order, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(somatcopy,SOMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, float* b, blasint* ldb, flexiblas_fortran_charlen_t len_order, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cimatcopy,CIMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, blasint* ldb, flexiblas_fortran_charlen_t len_order, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zimatcopy,ZIMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, blasint* ldb, flexiblas_fortran_charlen_t len_order, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dimatcopy,DIMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, blasint* ldb, flexiblas_fortran_charlen_t len_order, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(simatcopy,SIMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, blasint* ldb, flexiblas_fortran_charlen_t len_order, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sgeadd,SGEADD)(blasint* m, blasint* n, float* alpha, float* a, blasint* lda, float* beta, float* b, blasint* ldb);

    void FC_GLOBAL(dgeadd,DGEADD)(blasint* m, blasint* n, double* alpha, double* a, blasint* lda, double* beta, double* b, blasint* ldb);

    void FC_GLOBAL(cgeadd,CGEADD)(blasint* m, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* beta, float complex* b, blasint* ldb);

    void FC_GLOBAL(zgeadd,ZGEADD)(blasint* m, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* beta, double complex* b, blasint* ldb);


#ifdef __cplusplus
}
#endif
#endif
