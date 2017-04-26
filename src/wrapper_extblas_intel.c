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


void FC_GLOBAL(caxpby,CAXPBY)(blasint* n, float complex* ca, float complex* cx, blasint* incx, float complex* cb, float complex* cy, blasint* incy)
{
	double ts;
	void (*fn) (void* n, void* ca, void* cx, void* incx, void* cb, void* cy, void* incy);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.caxpby.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) n, (void*) ca, (void*) cx, (void*) incx, (void*) cb, (void*) cy, (void*) incy); 
		current_backend->extblas.caxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.caxpby.calls[0]++;
	} else { 
		fn((void*) n, (void*) ca, (void*) cx, (void*) incx, (void*) cb, (void*) cy, (void*) incy); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void caxpby_(blasint* n, float complex* ca, float complex* cx, blasint* incx, float complex* cb, float complex* cy, blasint* incy) __attribute__((alias(MTS(FC_GLOBAL(caxpby,CAXPBY)))));
#else
void caxpby(blasint* n, float complex* ca, float complex* cx, blasint* incx, float complex* cb, float complex* cy, blasint* incy) __attribute__((alias(MTS(FC_GLOBAL(caxpby,CAXPBY)))));
#endif



void FC_GLOBAL(daxpby,DAXPBY)(blasint* n, double* da, double* dx, blasint* incx, double* db, double* dy, blasint* incy)
{
	double ts;
	void (*fn) (void* n, void* da, void* dx, void* incx, void* db, void* dy, void* incy);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.daxpby.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) n, (void*) da, (void*) dx, (void*) incx, (void*) db, (void*) dy, (void*) incy); 
		current_backend->extblas.daxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.daxpby.calls[0]++;
	} else { 
		fn((void*) n, (void*) da, (void*) dx, (void*) incx, (void*) db, (void*) dy, (void*) incy); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void daxpby_(blasint* n, double* da, double* dx, blasint* incx, double* db, double* dy, blasint* incy) __attribute__((alias(MTS(FC_GLOBAL(daxpby,DAXPBY)))));
#else
void daxpby(blasint* n, double* da, double* dx, blasint* incx, double* db, double* dy, blasint* incy) __attribute__((alias(MTS(FC_GLOBAL(daxpby,DAXPBY)))));
#endif



void FC_GLOBAL(zaxpby,ZAXPBY)(blasint* n, double complex* za, double complex* zx, blasint* incx, double complex* zb, double complex* zy, blasint* incy)
{
	double ts;
	void (*fn) (void* n, void* za, void* zx, void* incx, void* zb, void* zy, void* incy);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zaxpby.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) n, (void*) za, (void*) zx, (void*) incx, (void*) zb, (void*) zy, (void*) incy); 
		current_backend->extblas.zaxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zaxpby.calls[0]++;
	} else { 
		fn((void*) n, (void*) za, (void*) zx, (void*) incx, (void*) zb, (void*) zy, (void*) incy); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void zaxpby_(blasint* n, double complex* za, double complex* zx, blasint* incx, double complex* zb, double complex* zy, blasint* incy) __attribute__((alias(MTS(FC_GLOBAL(zaxpby,ZAXPBY)))));
#else
void zaxpby(blasint* n, double complex* za, double complex* zx, blasint* incx, double complex* zb, double complex* zy, blasint* incy) __attribute__((alias(MTS(FC_GLOBAL(zaxpby,ZAXPBY)))));
#endif



void FC_GLOBAL(saxpby,SAXPBY)(blasint* n, float* sa, float* sx, blasint* incx, float* sb, float* sy, blasint* incy)
{
	double ts;
	void (*fn) (void* n, void* sa, void* sx, void* incx, void* sb, void* sy, void* incy);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.saxpby.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) n, (void*) sa, (void*) sx, (void*) incx, (void*) sb, (void*) sy, (void*) incy); 
		current_backend->extblas.saxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.saxpby.calls[0]++;
	} else { 
		fn((void*) n, (void*) sa, (void*) sx, (void*) incx, (void*) sb, (void*) sy, (void*) incy); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void saxpby_(blasint* n, float* sa, float* sx, blasint* incx, float* sb, float* sy, blasint* incy) __attribute__((alias(MTS(FC_GLOBAL(saxpby,SAXPBY)))));
#else
void saxpby(blasint* n, float* sa, float* sx, blasint* incx, float* sb, float* sy, blasint* incy) __attribute__((alias(MTS(FC_GLOBAL(saxpby,SAXPBY)))));
#endif



void FC_GLOBAL(comatcopy,COMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.comatcopy.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb); 
		current_backend->extblas.comatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.comatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void comatcopy_(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(comatcopy,COMATCOPY)))));
#else
void comatcopy(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(comatcopy,COMATCOPY)))));
#endif



void FC_GLOBAL(zomatcopy,ZOMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zomatcopy.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb); 
		current_backend->extblas.zomatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zomatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void zomatcopy_(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(zomatcopy,ZOMATCOPY)))));
#else
void zomatcopy(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(zomatcopy,ZOMATCOPY)))));
#endif



void FC_GLOBAL(domatcopy,DOMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, double* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.domatcopy.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb); 
		current_backend->extblas.domatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.domatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void domatcopy_(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, double* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(domatcopy,DOMATCOPY)))));
#else
void domatcopy(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, double* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(domatcopy,DOMATCOPY)))));
#endif



void FC_GLOBAL(somatcopy,SOMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, float* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.somatcopy.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb); 
		current_backend->extblas.somatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.somatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void somatcopy_(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, float* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(somatcopy,SOMATCOPY)))));
#else
void somatcopy(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, float* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(somatcopy,SOMATCOPY)))));
#endif



void FC_GLOBAL(cimatcopy,CIMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.cimatcopy.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb); 
		current_backend->extblas.cimatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.cimatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void cimatcopy_(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(cimatcopy,CIMATCOPY)))));
#else
void cimatcopy(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(cimatcopy,CIMATCOPY)))));
#endif



void FC_GLOBAL(zimatcopy,ZIMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zimatcopy.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb); 
		current_backend->extblas.zimatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zimatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void zimatcopy_(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(zimatcopy,ZIMATCOPY)))));
#else
void zimatcopy(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(zimatcopy,ZIMATCOPY)))));
#endif



void FC_GLOBAL(dimatcopy,DIMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.dimatcopy.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb); 
		current_backend->extblas.dimatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.dimatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void dimatcopy_(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(dimatcopy,DIMATCOPY)))));
#else
void dimatcopy(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(dimatcopy,DIMATCOPY)))));
#endif



void FC_GLOBAL(simatcopy,SIMATCOPY)(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.simatcopy.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb); 
		current_backend->extblas.simatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.simatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void simatcopy_(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(simatcopy,SIMATCOPY)))));
#else
void simatcopy(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(simatcopy,SIMATCOPY)))));
#endif



void FC_GLOBAL(sgeadd,SGEADD)(blasint* m, blasint* n, float* alpha, float* a, blasint* lda, float* beta, float* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.sgeadd.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb); 
		current_backend->extblas.sgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.sgeadd.calls[0]++;
	} else { 
		fn((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void sgeadd_(blasint* m, blasint* n, float* alpha, float* a, blasint* lda, float* beta, float* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(sgeadd,SGEADD)))));
#else
void sgeadd(blasint* m, blasint* n, float* alpha, float* a, blasint* lda, float* beta, float* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(sgeadd,SGEADD)))));
#endif



void FC_GLOBAL(dgeadd,DGEADD)(blasint* m, blasint* n, double* alpha, double* a, blasint* lda, double* beta, double* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.dgeadd.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb); 
		current_backend->extblas.dgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.dgeadd.calls[0]++;
	} else { 
		fn((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void dgeadd_(blasint* m, blasint* n, double* alpha, double* a, blasint* lda, double* beta, double* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(dgeadd,DGEADD)))));
#else
void dgeadd(blasint* m, blasint* n, double* alpha, double* a, blasint* lda, double* beta, double* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(dgeadd,DGEADD)))));
#endif



void FC_GLOBAL(cgeadd,CGEADD)(blasint* m, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* beta, float complex* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.cgeadd.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb); 
		current_backend->extblas.cgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.cgeadd.calls[0]++;
	} else { 
		fn((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void cgeadd_(blasint* m, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* beta, float complex* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(cgeadd,CGEADD)))));
#else
void cgeadd(blasint* m, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* beta, float complex* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(cgeadd,CGEADD)))));
#endif



void FC_GLOBAL(zgeadd,ZGEADD)(blasint* m, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* beta, double complex* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zgeadd.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb); 
		current_backend->extblas.zgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zgeadd.calls[0]++;
	} else { 
		fn((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void zgeadd_(blasint* m, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* beta, double complex* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(zgeadd,ZGEADD)))));
#else
void zgeadd(blasint* m, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* beta, double complex* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(zgeadd,ZGEADD)))));
#endif



