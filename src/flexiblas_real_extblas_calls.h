/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2015-2016
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Tue Mar  7 10:13:38 2017 */ 
        
#ifndef FLEXIBLAS_REAL_CALLS_H
#define FLEXIBLAS_REAL_CALLS_H

#include <stdint.h>

#include "fortran_mangle.h"
#include <complex.h>

#ifdef __cplusplus
extern "C" {
#endif

void flexiblas_real_caxpby_(void* n, void* ca, void* cx, void* incx, void* cb, void* cy, void* incy);
void flexiblas_real_caxpby(void* n, void* ca, void* cx, void* incx, void* cb, void* cy, void* incy);
void flexiblas_real_daxpby_(void* n, void* da, void* dx, void* incx, void* db, void* dy, void* incy);
void flexiblas_real_daxpby(void* n, void* da, void* dx, void* incx, void* db, void* dy, void* incy);
void flexiblas_real_zaxpby_(void* n, void* za, void* zx, void* incx, void* zb, void* zy, void* incy);
void flexiblas_real_zaxpby(void* n, void* za, void* zx, void* incx, void* zb, void* zy, void* incy);
void flexiblas_real_saxpby_(void* n, void* sa, void* sx, void* incx, void* sb, void* sy, void* incy);
void flexiblas_real_saxpby(void* n, void* sa, void* sx, void* incx, void* sb, void* sy, void* incy);
void flexiblas_real_comatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
void flexiblas_real_comatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
void flexiblas_real_zomatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
void flexiblas_real_zomatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
void flexiblas_real_domatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
void flexiblas_real_domatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
void flexiblas_real_somatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
void flexiblas_real_somatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
void flexiblas_real_cimatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
void flexiblas_real_cimatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
void flexiblas_real_zimatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
void flexiblas_real_zimatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
void flexiblas_real_dimatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
void flexiblas_real_dimatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
void flexiblas_real_simatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
void flexiblas_real_simatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
void flexiblas_real_sgeadd_(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
void flexiblas_real_sgeadd(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
void flexiblas_real_dgeadd_(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
void flexiblas_real_dgeadd(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
void flexiblas_real_cgeadd_(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
void flexiblas_real_cgeadd(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
void flexiblas_real_zgeadd_(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
void flexiblas_real_zgeadd(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);

#ifdef __cplusplus
}
#endif
#endif
