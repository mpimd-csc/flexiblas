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
        
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "fortran_mangle.h"

#include "flexiblas.h"



#ifdef INTEGER8
#define blasint int64_t
#else 
#define blasint int 
#endif
#ifndef __INT32_MAX__
#define __INT32_MAX__ 2147483647
#endif 


void flexiblas_real_caxpby_(void* n, void* ca, void* cx, void* incx, void* cb, void* cy, void* incy)
{
	void (*fn) (void* n, void* ca, void* cx, void* incx, void* cb, void* cy, void* incy);

	fn = current_backend->extblas.caxpby.fblas_real; 

		fn((void*) n, (void*) ca, (void*) cx, (void*) incx, (void*) cb, (void*) cy, (void*) incy); 

	return;
}
void flexiblas_real_caxpby(void* n, void* ca, void* cx, void* incx, void* cb, void* cy, void* incy) __attribute__((alias("flexiblas_real_caxpby_")));



void flexiblas_real_daxpby_(void* n, void* da, void* dx, void* incx, void* db, void* dy, void* incy)
{
	void (*fn) (void* n, void* da, void* dx, void* incx, void* db, void* dy, void* incy);

	fn = current_backend->extblas.daxpby.fblas_real; 

		fn((void*) n, (void*) da, (void*) dx, (void*) incx, (void*) db, (void*) dy, (void*) incy); 

	return;
}
void flexiblas_real_daxpby(void* n, void* da, void* dx, void* incx, void* db, void* dy, void* incy) __attribute__((alias("flexiblas_real_daxpby_")));



void flexiblas_real_zaxpby_(void* n, void* za, void* zx, void* incx, void* zb, void* zy, void* incy)
{
	void (*fn) (void* n, void* za, void* zx, void* incx, void* zb, void* zy, void* incy);

	fn = current_backend->extblas.zaxpby.fblas_real; 

		fn((void*) n, (void*) za, (void*) zx, (void*) incx, (void*) zb, (void*) zy, (void*) incy); 

	return;
}
void flexiblas_real_zaxpby(void* n, void* za, void* zx, void* incx, void* zb, void* zy, void* incy) __attribute__((alias("flexiblas_real_zaxpby_")));



void flexiblas_real_saxpby_(void* n, void* sa, void* sx, void* incx, void* sb, void* sy, void* incy)
{
	void (*fn) (void* n, void* sa, void* sx, void* incx, void* sb, void* sy, void* incy);

	fn = current_backend->extblas.saxpby.fblas_real; 

		fn((void*) n, (void*) sa, (void*) sx, (void*) incx, (void*) sb, (void*) sy, (void*) incy); 

	return;
}
void flexiblas_real_saxpby(void* n, void* sa, void* sx, void* incx, void* sb, void* sy, void* incy) __attribute__((alias("flexiblas_real_saxpby_")));



void flexiblas_real_comatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb)
{
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);

	fn = current_backend->extblas.comatcopy.fblas_real; 

		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb); 

	return;
}
void flexiblas_real_comatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb) __attribute__((alias("flexiblas_real_comatcopy_")));



void flexiblas_real_zomatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb)
{
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);

	fn = current_backend->extblas.zomatcopy.fblas_real; 

		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb); 

	return;
}
void flexiblas_real_zomatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb) __attribute__((alias("flexiblas_real_zomatcopy_")));



void flexiblas_real_domatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb)
{
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);

	fn = current_backend->extblas.domatcopy.fblas_real; 

		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb); 

	return;
}
void flexiblas_real_domatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb) __attribute__((alias("flexiblas_real_domatcopy_")));



void flexiblas_real_somatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb)
{
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);

	fn = current_backend->extblas.somatcopy.fblas_real; 

		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb); 

	return;
}
void flexiblas_real_somatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb) __attribute__((alias("flexiblas_real_somatcopy_")));



void flexiblas_real_cimatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb)
{
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);

	fn = current_backend->extblas.cimatcopy.fblas_real; 

		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb); 

	return;
}
void flexiblas_real_cimatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb) __attribute__((alias("flexiblas_real_cimatcopy_")));



void flexiblas_real_zimatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb)
{
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);

	fn = current_backend->extblas.zimatcopy.fblas_real; 

		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb); 

	return;
}
void flexiblas_real_zimatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb) __attribute__((alias("flexiblas_real_zimatcopy_")));



void flexiblas_real_dimatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb)
{
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);

	fn = current_backend->extblas.dimatcopy.fblas_real; 

		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb); 

	return;
}
void flexiblas_real_dimatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb) __attribute__((alias("flexiblas_real_dimatcopy_")));



void flexiblas_real_simatcopy_(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb)
{
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);

	fn = current_backend->extblas.simatcopy.fblas_real; 

		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb); 

	return;
}
void flexiblas_real_simatcopy(void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb) __attribute__((alias("flexiblas_real_simatcopy_")));



void flexiblas_real_sgeadd_(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb)
{
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);

	fn = current_backend->extblas.sgeadd.fblas_real; 

		fn((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb); 

	return;
}
void flexiblas_real_sgeadd(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb) __attribute__((alias("flexiblas_real_sgeadd_")));



void flexiblas_real_dgeadd_(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb)
{
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);

	fn = current_backend->extblas.dgeadd.fblas_real; 

		fn((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb); 

	return;
}
void flexiblas_real_dgeadd(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb) __attribute__((alias("flexiblas_real_dgeadd_")));



void flexiblas_real_cgeadd_(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb)
{
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);

	fn = current_backend->extblas.cgeadd.fblas_real; 

		fn((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb); 

	return;
}
void flexiblas_real_cgeadd(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb) __attribute__((alias("flexiblas_real_cgeadd_")));



void flexiblas_real_zgeadd_(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb)
{
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);

	fn = current_backend->extblas.zgeadd.fblas_real; 

		fn((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb); 

	return;
}
void flexiblas_real_zgeadd(void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb) __attribute__((alias("flexiblas_real_zgeadd_")));



