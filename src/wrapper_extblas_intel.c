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
 * Copyright (C) Martin Koehler, 2015
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Thu Sep  3 16:30:00 2015 */ 
        
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas.h"


#ifdef INTEGER8
#define blasint int64_t
#else 
#define blasint int 
#endif
#ifndef __INT32_MAX__
#define __INT32_MAX__ 2147483647
#endif 


void caxpby_(blasint* n, float complex* ca, float complex* cx, blasint* incx, float complex* cb, float complex* cy, blasint* incy)
{
	double ts;
	void (*fn) (void* n, void* ca, void* cx, void* incx, void* cb, void* cy, void* incy);
	int32_t _n32; int64_t _n64; void* _pn;
	int32_t _incx32; int64_t _incx64; void* _pincx;
	int32_t _incy32; int64_t _incy64; void* _pincy;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.caxpby.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "caxpby_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_pn = n;
		_pincx = incx;
		_pincy = incy;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of caxpby the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incx > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of caxpby the parameter incx is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incy > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of caxpby the parameter incy is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_incx32 = (int32_t) *incx;
		_pincx = &_incx32;
		_incy32 = (int32_t) *incy;
		_pincy = &_incy32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_incx64 = (int64_t)*incx;
		_pincx = &_incx64;
		_incy64 = (int64_t)*incy;
		_pincy = &_incy64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "caxpby - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pn, (void*) ca, (void*) cx, (void*) _pincx, (void*) cb, (void*) cy, (void*) _pincy); 
		current_backend->extblas.caxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.caxpby.calls[0]++;
	} else { 
		fn((void*) _pn, (void*) ca, (void*) cx, (void*) _pincx, (void*) cb, (void*) cy, (void*) _pincy); 
	} 
	return;
}
void caxpby(blasint* n, float complex* ca, float complex* cx, blasint* incx, float complex* cb, float complex* cy, blasint* incy) __attribute__((alias("caxpby_")));


void caxpby32_(int32_t* n, float complex* ca, float complex* cx, int32_t* incx, float complex* cb, float complex* cy, int32_t* incy)
{
	double ts;
	void (*fn) (void* n, void* ca, void* cx, void* incx, void* cb, void* cy, void* incy);
	int64_t _n64; void* _pn;
	int64_t _incx64; void* _pincx;
	int64_t _incy64; void* _pincy;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.caxpby.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "caxpby_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_pn = n;
		_pincx = incx;
		_pincy = incy;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_incx64 = (int64_t)*incx;
		_pincx = &_incx64;
		_incy64 = (int64_t)*incy;
		_pincy = &_incy64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "caxpby32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pn, (void*) ca, (void*) cx, (void*) _pincx, (void*) cb, (void*) cy, (void*) _pincy); 
		current_backend->extblas.caxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.caxpby.calls[0]++;
	} else { 
		fn((void*) _pn, (void*) ca, (void*) cx, (void*) _pincx, (void*) cb, (void*) cy, (void*) _pincy); 
	} 
	return;
}
void caxpby32(int32_t* n, float complex* ca, float complex* cx, int32_t* incx, float complex* cb, float complex* cy, int32_t* incy) __attribute__((alias("caxpby32_")));


void caxpby64_(int64_t* n, float complex* ca, float complex* cx, int64_t* incx, float complex* cb, float complex* cy, int64_t* incy)
{
	double ts;
	void (*fn) (void* n, void* ca, void* cx, void* incx, void* cb, void* cy, void* incy);
	int32_t _n32; void* _pn;
	int32_t _incx32; void* _pincx;
	int32_t _incy32; void* _pincy;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.caxpby.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "caxpby_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_pn = n;
		_pincx = incx;
		_pincy = incy;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of caxpby64 the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incx > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of caxpby64 the parameter incx is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incy > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of caxpby64 the parameter incy is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_incx32 = (int32_t) *incx;
		_pincx = &_incx32;
		_incy32 = (int32_t) *incy;
		_pincy = &_incy32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "caxpby64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pn, (void*) ca, (void*) cx, (void*) _pincx, (void*) cb, (void*) cy, (void*) _pincy); 
		current_backend->extblas.caxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.caxpby.calls[0]++;
	} else { 
		fn((void*) _pn, (void*) ca, (void*) cx, (void*) _pincx, (void*) cb, (void*) cy, (void*) _pincy); 
	} 
	return;
}
void caxpby64(int64_t* n, float complex* ca, float complex* cx, int64_t* incx, float complex* cb, float complex* cy, int64_t* incy) __attribute__((alias("caxpby64_")));



void daxpby_(blasint* n, double* da, double* dx, blasint* incx, double* db, double* dy, blasint* incy)
{
	double ts;
	void (*fn) (void* n, void* da, void* dx, void* incx, void* db, void* dy, void* incy);
	int32_t _n32; int64_t _n64; void* _pn;
	int32_t _incx32; int64_t _incx64; void* _pincx;
	int32_t _incy32; int64_t _incy64; void* _pincy;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.daxpby.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "daxpby_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_pn = n;
		_pincx = incx;
		_pincy = incy;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of daxpby the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incx > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of daxpby the parameter incx is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incy > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of daxpby the parameter incy is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_incx32 = (int32_t) *incx;
		_pincx = &_incx32;
		_incy32 = (int32_t) *incy;
		_pincy = &_incy32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_incx64 = (int64_t)*incx;
		_pincx = &_incx64;
		_incy64 = (int64_t)*incy;
		_pincy = &_incy64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "daxpby - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pn, (void*) da, (void*) dx, (void*) _pincx, (void*) db, (void*) dy, (void*) _pincy); 
		current_backend->extblas.daxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.daxpby.calls[0]++;
	} else { 
		fn((void*) _pn, (void*) da, (void*) dx, (void*) _pincx, (void*) db, (void*) dy, (void*) _pincy); 
	} 
	return;
}
void daxpby(blasint* n, double* da, double* dx, blasint* incx, double* db, double* dy, blasint* incy) __attribute__((alias("daxpby_")));


void daxpby32_(int32_t* n, double* da, double* dx, int32_t* incx, double* db, double* dy, int32_t* incy)
{
	double ts;
	void (*fn) (void* n, void* da, void* dx, void* incx, void* db, void* dy, void* incy);
	int64_t _n64; void* _pn;
	int64_t _incx64; void* _pincx;
	int64_t _incy64; void* _pincy;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.daxpby.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "daxpby_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_pn = n;
		_pincx = incx;
		_pincy = incy;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_incx64 = (int64_t)*incx;
		_pincx = &_incx64;
		_incy64 = (int64_t)*incy;
		_pincy = &_incy64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "daxpby32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pn, (void*) da, (void*) dx, (void*) _pincx, (void*) db, (void*) dy, (void*) _pincy); 
		current_backend->extblas.daxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.daxpby.calls[0]++;
	} else { 
		fn((void*) _pn, (void*) da, (void*) dx, (void*) _pincx, (void*) db, (void*) dy, (void*) _pincy); 
	} 
	return;
}
void daxpby32(int32_t* n, double* da, double* dx, int32_t* incx, double* db, double* dy, int32_t* incy) __attribute__((alias("daxpby32_")));


void daxpby64_(int64_t* n, double* da, double* dx, int64_t* incx, double* db, double* dy, int64_t* incy)
{
	double ts;
	void (*fn) (void* n, void* da, void* dx, void* incx, void* db, void* dy, void* incy);
	int32_t _n32; void* _pn;
	int32_t _incx32; void* _pincx;
	int32_t _incy32; void* _pincy;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.daxpby.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "daxpby_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_pn = n;
		_pincx = incx;
		_pincy = incy;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of daxpby64 the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incx > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of daxpby64 the parameter incx is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incy > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of daxpby64 the parameter incy is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_incx32 = (int32_t) *incx;
		_pincx = &_incx32;
		_incy32 = (int32_t) *incy;
		_pincy = &_incy32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "daxpby64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pn, (void*) da, (void*) dx, (void*) _pincx, (void*) db, (void*) dy, (void*) _pincy); 
		current_backend->extblas.daxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.daxpby.calls[0]++;
	} else { 
		fn((void*) _pn, (void*) da, (void*) dx, (void*) _pincx, (void*) db, (void*) dy, (void*) _pincy); 
	} 
	return;
}
void daxpby64(int64_t* n, double* da, double* dx, int64_t* incx, double* db, double* dy, int64_t* incy) __attribute__((alias("daxpby64_")));



void zaxpby_(blasint* n, double complex* za, double complex* zx, blasint* incx, double complex* zb, double complex* zy, blasint* incy)
{
	double ts;
	void (*fn) (void* n, void* za, void* zx, void* incx, void* zb, void* zy, void* incy);
	int32_t _n32; int64_t _n64; void* _pn;
	int32_t _incx32; int64_t _incx64; void* _pincx;
	int32_t _incy32; int64_t _incy64; void* _pincy;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zaxpby.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "zaxpby_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_pn = n;
		_pincx = incx;
		_pincy = incy;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zaxpby the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incx > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zaxpby the parameter incx is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incy > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zaxpby the parameter incy is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_incx32 = (int32_t) *incx;
		_pincx = &_incx32;
		_incy32 = (int32_t) *incy;
		_pincy = &_incy32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_incx64 = (int64_t)*incx;
		_pincx = &_incx64;
		_incy64 = (int64_t)*incy;
		_pincy = &_incy64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "zaxpby - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pn, (void*) za, (void*) zx, (void*) _pincx, (void*) zb, (void*) zy, (void*) _pincy); 
		current_backend->extblas.zaxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zaxpby.calls[0]++;
	} else { 
		fn((void*) _pn, (void*) za, (void*) zx, (void*) _pincx, (void*) zb, (void*) zy, (void*) _pincy); 
	} 
	return;
}
void zaxpby(blasint* n, double complex* za, double complex* zx, blasint* incx, double complex* zb, double complex* zy, blasint* incy) __attribute__((alias("zaxpby_")));


void zaxpby32_(int32_t* n, double complex* za, double complex* zx, int32_t* incx, double complex* zb, double complex* zy, int32_t* incy)
{
	double ts;
	void (*fn) (void* n, void* za, void* zx, void* incx, void* zb, void* zy, void* incy);
	int64_t _n64; void* _pn;
	int64_t _incx64; void* _pincx;
	int64_t _incy64; void* _pincy;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zaxpby.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "zaxpby_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_pn = n;
		_pincx = incx;
		_pincy = incy;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_incx64 = (int64_t)*incx;
		_pincx = &_incx64;
		_incy64 = (int64_t)*incy;
		_pincy = &_incy64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "zaxpby32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pn, (void*) za, (void*) zx, (void*) _pincx, (void*) zb, (void*) zy, (void*) _pincy); 
		current_backend->extblas.zaxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zaxpby.calls[0]++;
	} else { 
		fn((void*) _pn, (void*) za, (void*) zx, (void*) _pincx, (void*) zb, (void*) zy, (void*) _pincy); 
	} 
	return;
}
void zaxpby32(int32_t* n, double complex* za, double complex* zx, int32_t* incx, double complex* zb, double complex* zy, int32_t* incy) __attribute__((alias("zaxpby32_")));


void zaxpby64_(int64_t* n, double complex* za, double complex* zx, int64_t* incx, double complex* zb, double complex* zy, int64_t* incy)
{
	double ts;
	void (*fn) (void* n, void* za, void* zx, void* incx, void* zb, void* zy, void* incy);
	int32_t _n32; void* _pn;
	int32_t _incx32; void* _pincx;
	int32_t _incy32; void* _pincy;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zaxpby.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "zaxpby_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_pn = n;
		_pincx = incx;
		_pincy = incy;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zaxpby64 the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incx > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zaxpby64 the parameter incx is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incy > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zaxpby64 the parameter incy is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_incx32 = (int32_t) *incx;
		_pincx = &_incx32;
		_incy32 = (int32_t) *incy;
		_pincy = &_incy32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "zaxpby64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pn, (void*) za, (void*) zx, (void*) _pincx, (void*) zb, (void*) zy, (void*) _pincy); 
		current_backend->extblas.zaxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zaxpby.calls[0]++;
	} else { 
		fn((void*) _pn, (void*) za, (void*) zx, (void*) _pincx, (void*) zb, (void*) zy, (void*) _pincy); 
	} 
	return;
}
void zaxpby64(int64_t* n, double complex* za, double complex* zx, int64_t* incx, double complex* zb, double complex* zy, int64_t* incy) __attribute__((alias("zaxpby64_")));



void saxpby_(blasint* n, float* sa, float* sx, blasint* incx, float* sb, float* sy, blasint* incy)
{
	double ts;
	void (*fn) (void* n, void* sa, void* sx, void* incx, void* sb, void* sy, void* incy);
	int32_t _n32; int64_t _n64; void* _pn;
	int32_t _incx32; int64_t _incx64; void* _pincx;
	int32_t _incy32; int64_t _incy64; void* _pincy;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.saxpby.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "saxpby_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_pn = n;
		_pincx = incx;
		_pincy = incy;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of saxpby the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incx > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of saxpby the parameter incx is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incy > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of saxpby the parameter incy is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_incx32 = (int32_t) *incx;
		_pincx = &_incx32;
		_incy32 = (int32_t) *incy;
		_pincy = &_incy32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_incx64 = (int64_t)*incx;
		_pincx = &_incx64;
		_incy64 = (int64_t)*incy;
		_pincy = &_incy64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "saxpby - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pn, (void*) sa, (void*) sx, (void*) _pincx, (void*) sb, (void*) sy, (void*) _pincy); 
		current_backend->extblas.saxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.saxpby.calls[0]++;
	} else { 
		fn((void*) _pn, (void*) sa, (void*) sx, (void*) _pincx, (void*) sb, (void*) sy, (void*) _pincy); 
	} 
	return;
}
void saxpby(blasint* n, float* sa, float* sx, blasint* incx, float* sb, float* sy, blasint* incy) __attribute__((alias("saxpby_")));


void saxpby32_(int32_t* n, float* sa, float* sx, int32_t* incx, float* sb, float* sy, int32_t* incy)
{
	double ts;
	void (*fn) (void* n, void* sa, void* sx, void* incx, void* sb, void* sy, void* incy);
	int64_t _n64; void* _pn;
	int64_t _incx64; void* _pincx;
	int64_t _incy64; void* _pincy;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.saxpby.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "saxpby_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_pn = n;
		_pincx = incx;
		_pincy = incy;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_incx64 = (int64_t)*incx;
		_pincx = &_incx64;
		_incy64 = (int64_t)*incy;
		_pincy = &_incy64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "saxpby32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pn, (void*) sa, (void*) sx, (void*) _pincx, (void*) sb, (void*) sy, (void*) _pincy); 
		current_backend->extblas.saxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.saxpby.calls[0]++;
	} else { 
		fn((void*) _pn, (void*) sa, (void*) sx, (void*) _pincx, (void*) sb, (void*) sy, (void*) _pincy); 
	} 
	return;
}
void saxpby32(int32_t* n, float* sa, float* sx, int32_t* incx, float* sb, float* sy, int32_t* incy) __attribute__((alias("saxpby32_")));


void saxpby64_(int64_t* n, float* sa, float* sx, int64_t* incx, float* sb, float* sy, int64_t* incy)
{
	double ts;
	void (*fn) (void* n, void* sa, void* sx, void* incx, void* sb, void* sy, void* incy);
	int32_t _n32; void* _pn;
	int32_t _incx32; void* _pincx;
	int32_t _incy32; void* _pincy;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.saxpby.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "saxpby_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_pn = n;
		_pincx = incx;
		_pincy = incy;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of saxpby64 the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incx > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of saxpby64 the parameter incx is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *incy > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of saxpby64 the parameter incy is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_incx32 = (int32_t) *incx;
		_pincx = &_incx32;
		_incy32 = (int32_t) *incy;
		_pincy = &_incy32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "saxpby64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pn, (void*) sa, (void*) sx, (void*) _pincx, (void*) sb, (void*) sy, (void*) _pincy); 
		current_backend->extblas.saxpby.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.saxpby.calls[0]++;
	} else { 
		fn((void*) _pn, (void*) sa, (void*) sx, (void*) _pincx, (void*) sb, (void*) sy, (void*) _pincy); 
	} 
	return;
}
void saxpby64(int64_t* n, float* sa, float* sx, int64_t* incx, float* sb, float* sy, int64_t* incy) __attribute__((alias("saxpby64_")));



void comatcopy_(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	int32_t _rows32; int64_t _rows64; void* _prows;
	int32_t _cols32; int64_t _cols64; void* _pcols;
	int32_t _lda32; int64_t _lda64; void* _plda;
	int32_t _ldb32; int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.comatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "comatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of comatcopy the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of comatcopy the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of comatcopy the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of comatcopy the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "comatcopy - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
		current_backend->extblas.comatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.comatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
	} 
	return;
}
void comatcopy(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb) __attribute__((alias("comatcopy_")));


void comatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, float complex* alpha, float complex* a, int32_t* lda, float complex* b, int32_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	int64_t _rows64; void* _prows;
	int64_t _cols64; void* _pcols;
	int64_t _lda64; void* _plda;
	int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.comatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "comatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "comatcopy32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
		current_backend->extblas.comatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.comatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
	} 
	return;
}
void comatcopy32(char* order, char* trans, int32_t* rows, int32_t* cols, float complex* alpha, float complex* a, int32_t* lda, float complex* b, int32_t* ldb) __attribute__((alias("comatcopy32_")));


void comatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, float complex* alpha, float complex* a, int64_t* lda, float complex* b, int64_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	int32_t _rows32; void* _prows;
	int32_t _cols32; void* _pcols;
	int32_t _lda32; void* _plda;
	int32_t _ldb32; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.comatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "comatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of comatcopy64 the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of comatcopy64 the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of comatcopy64 the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of comatcopy64 the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "comatcopy64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
		current_backend->extblas.comatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.comatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
	} 
	return;
}
void comatcopy64(char* order, char* trans, int64_t* rows, int64_t* cols, float complex* alpha, float complex* a, int64_t* lda, float complex* b, int64_t* ldb) __attribute__((alias("comatcopy64_")));



void zomatcopy_(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	int32_t _rows32; int64_t _rows64; void* _prows;
	int32_t _cols32; int64_t _cols64; void* _pcols;
	int32_t _lda32; int64_t _lda64; void* _plda;
	int32_t _ldb32; int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zomatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "zomatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zomatcopy the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zomatcopy the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zomatcopy the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zomatcopy the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "zomatcopy - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
		current_backend->extblas.zomatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zomatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
	} 
	return;
}
void zomatcopy(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb) __attribute__((alias("zomatcopy_")));


void zomatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, double complex* alpha, double complex* a, int32_t* lda, double complex* b, int32_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	int64_t _rows64; void* _prows;
	int64_t _cols64; void* _pcols;
	int64_t _lda64; void* _plda;
	int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zomatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "zomatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "zomatcopy32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
		current_backend->extblas.zomatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zomatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
	} 
	return;
}
void zomatcopy32(char* order, char* trans, int32_t* rows, int32_t* cols, double complex* alpha, double complex* a, int32_t* lda, double complex* b, int32_t* ldb) __attribute__((alias("zomatcopy32_")));


void zomatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, double complex* alpha, double complex* a, int64_t* lda, double complex* b, int64_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	int32_t _rows32; void* _prows;
	int32_t _cols32; void* _pcols;
	int32_t _lda32; void* _plda;
	int32_t _ldb32; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zomatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "zomatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zomatcopy64 the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zomatcopy64 the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zomatcopy64 the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zomatcopy64 the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "zomatcopy64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
		current_backend->extblas.zomatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zomatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
	} 
	return;
}
void zomatcopy64(char* order, char* trans, int64_t* rows, int64_t* cols, double complex* alpha, double complex* a, int64_t* lda, double complex* b, int64_t* ldb) __attribute__((alias("zomatcopy64_")));



void domatcopy_(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, double* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	int32_t _rows32; int64_t _rows64; void* _prows;
	int32_t _cols32; int64_t _cols64; void* _pcols;
	int32_t _lda32; int64_t _lda64; void* _plda;
	int32_t _ldb32; int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.domatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "domatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of domatcopy the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of domatcopy the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of domatcopy the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of domatcopy the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "domatcopy - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
		current_backend->extblas.domatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.domatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
	} 
	return;
}
void domatcopy(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, double* b, blasint* ldb) __attribute__((alias("domatcopy_")));


void domatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, double* alpha, double* a, int32_t* lda, double* b, int32_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	int64_t _rows64; void* _prows;
	int64_t _cols64; void* _pcols;
	int64_t _lda64; void* _plda;
	int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.domatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "domatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "domatcopy32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
		current_backend->extblas.domatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.domatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
	} 
	return;
}
void domatcopy32(char* order, char* trans, int32_t* rows, int32_t* cols, double* alpha, double* a, int32_t* lda, double* b, int32_t* ldb) __attribute__((alias("domatcopy32_")));


void domatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, double* alpha, double* a, int64_t* lda, double* b, int64_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	int32_t _rows32; void* _prows;
	int32_t _cols32; void* _pcols;
	int32_t _lda32; void* _plda;
	int32_t _ldb32; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.domatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "domatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of domatcopy64 the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of domatcopy64 the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of domatcopy64 the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of domatcopy64 the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "domatcopy64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
		current_backend->extblas.domatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.domatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
	} 
	return;
}
void domatcopy64(char* order, char* trans, int64_t* rows, int64_t* cols, double* alpha, double* a, int64_t* lda, double* b, int64_t* ldb) __attribute__((alias("domatcopy64_")));



void somatcopy_(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, float* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	int32_t _rows32; int64_t _rows64; void* _prows;
	int32_t _cols32; int64_t _cols64; void* _pcols;
	int32_t _lda32; int64_t _lda64; void* _plda;
	int32_t _ldb32; int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.somatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "somatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of somatcopy the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of somatcopy the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of somatcopy the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of somatcopy the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "somatcopy - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
		current_backend->extblas.somatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.somatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
	} 
	return;
}
void somatcopy(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, float* b, blasint* ldb) __attribute__((alias("somatcopy_")));


void somatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, float* alpha, float* a, int32_t* lda, float* b, int32_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	int64_t _rows64; void* _prows;
	int64_t _cols64; void* _pcols;
	int64_t _lda64; void* _plda;
	int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.somatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "somatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "somatcopy32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
		current_backend->extblas.somatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.somatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
	} 
	return;
}
void somatcopy32(char* order, char* trans, int32_t* rows, int32_t* cols, float* alpha, float* a, int32_t* lda, float* b, int32_t* ldb) __attribute__((alias("somatcopy32_")));


void somatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, float* alpha, float* a, int64_t* lda, float* b, int64_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* b, void* ldb);
	int32_t _rows32; void* _prows;
	int32_t _cols32; void* _pcols;
	int32_t _lda32; void* _plda;
	int32_t _ldb32; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.somatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "somatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of somatcopy64 the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of somatcopy64 the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of somatcopy64 the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of somatcopy64 the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "somatcopy64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
		current_backend->extblas.somatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.somatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) b, (void*) _pldb); 
	} 
	return;
}
void somatcopy64(char* order, char* trans, int64_t* rows, int64_t* cols, float* alpha, float* a, int64_t* lda, float* b, int64_t* ldb) __attribute__((alias("somatcopy64_")));



void cimatcopy_(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	int32_t _rows32; int64_t _rows64; void* _prows;
	int32_t _cols32; int64_t _cols64; void* _pcols;
	int32_t _lda32; int64_t _lda64; void* _plda;
	int32_t _ldb32; int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.cimatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "cimatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cimatcopy the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cimatcopy the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cimatcopy the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cimatcopy the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "cimatcopy - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
		current_backend->extblas.cimatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.cimatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
	} 
	return;
}
void cimatcopy(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, blasint* ldb) __attribute__((alias("cimatcopy_")));


void cimatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, float complex* alpha, float complex* a, int32_t* lda, int32_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	int64_t _rows64; void* _prows;
	int64_t _cols64; void* _pcols;
	int64_t _lda64; void* _plda;
	int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.cimatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "cimatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "cimatcopy32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
		current_backend->extblas.cimatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.cimatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
	} 
	return;
}
void cimatcopy32(char* order, char* trans, int32_t* rows, int32_t* cols, float complex* alpha, float complex* a, int32_t* lda, int32_t* ldb) __attribute__((alias("cimatcopy32_")));


void cimatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, float complex* alpha, float complex* a, int64_t* lda, int64_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	int32_t _rows32; void* _prows;
	int32_t _cols32; void* _pcols;
	int32_t _lda32; void* _plda;
	int32_t _ldb32; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.cimatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "cimatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cimatcopy64 the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cimatcopy64 the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cimatcopy64 the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cimatcopy64 the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "cimatcopy64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
		current_backend->extblas.cimatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.cimatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
	} 
	return;
}
void cimatcopy64(char* order, char* trans, int64_t* rows, int64_t* cols, float complex* alpha, float complex* a, int64_t* lda, int64_t* ldb) __attribute__((alias("cimatcopy64_")));



void zimatcopy_(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	int32_t _rows32; int64_t _rows64; void* _prows;
	int32_t _cols32; int64_t _cols64; void* _pcols;
	int32_t _lda32; int64_t _lda64; void* _plda;
	int32_t _ldb32; int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zimatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "zimatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zimatcopy the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zimatcopy the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zimatcopy the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zimatcopy the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "zimatcopy - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
		current_backend->extblas.zimatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zimatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
	} 
	return;
}
void zimatcopy(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, blasint* ldb) __attribute__((alias("zimatcopy_")));


void zimatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, double complex* alpha, double complex* a, int32_t* lda, int32_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	int64_t _rows64; void* _prows;
	int64_t _cols64; void* _pcols;
	int64_t _lda64; void* _plda;
	int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zimatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "zimatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "zimatcopy32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
		current_backend->extblas.zimatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zimatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
	} 
	return;
}
void zimatcopy32(char* order, char* trans, int32_t* rows, int32_t* cols, double complex* alpha, double complex* a, int32_t* lda, int32_t* ldb) __attribute__((alias("zimatcopy32_")));


void zimatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, double complex* alpha, double complex* a, int64_t* lda, int64_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	int32_t _rows32; void* _prows;
	int32_t _cols32; void* _pcols;
	int32_t _lda32; void* _plda;
	int32_t _ldb32; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zimatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "zimatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zimatcopy64 the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zimatcopy64 the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zimatcopy64 the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zimatcopy64 the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "zimatcopy64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
		current_backend->extblas.zimatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zimatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
	} 
	return;
}
void zimatcopy64(char* order, char* trans, int64_t* rows, int64_t* cols, double complex* alpha, double complex* a, int64_t* lda, int64_t* ldb) __attribute__((alias("zimatcopy64_")));



void dimatcopy_(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	int32_t _rows32; int64_t _rows64; void* _prows;
	int32_t _cols32; int64_t _cols64; void* _pcols;
	int32_t _lda32; int64_t _lda64; void* _plda;
	int32_t _ldb32; int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.dimatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "dimatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dimatcopy the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dimatcopy the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dimatcopy the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dimatcopy the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "dimatcopy - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
		current_backend->extblas.dimatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.dimatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
	} 
	return;
}
void dimatcopy(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, blasint* ldb) __attribute__((alias("dimatcopy_")));


void dimatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, double* alpha, double* a, int32_t* lda, int32_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	int64_t _rows64; void* _prows;
	int64_t _cols64; void* _pcols;
	int64_t _lda64; void* _plda;
	int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.dimatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "dimatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "dimatcopy32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
		current_backend->extblas.dimatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.dimatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
	} 
	return;
}
void dimatcopy32(char* order, char* trans, int32_t* rows, int32_t* cols, double* alpha, double* a, int32_t* lda, int32_t* ldb) __attribute__((alias("dimatcopy32_")));


void dimatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, double* alpha, double* a, int64_t* lda, int64_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	int32_t _rows32; void* _prows;
	int32_t _cols32; void* _pcols;
	int32_t _lda32; void* _plda;
	int32_t _ldb32; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.dimatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "dimatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dimatcopy64 the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dimatcopy64 the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dimatcopy64 the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dimatcopy64 the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "dimatcopy64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
		current_backend->extblas.dimatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.dimatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
	} 
	return;
}
void dimatcopy64(char* order, char* trans, int64_t* rows, int64_t* cols, double* alpha, double* a, int64_t* lda, int64_t* ldb) __attribute__((alias("dimatcopy64_")));



void simatcopy_(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, blasint* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	int32_t _rows32; int64_t _rows64; void* _prows;
	int32_t _cols32; int64_t _cols64; void* _pcols;
	int32_t _lda32; int64_t _lda64; void* _plda;
	int32_t _ldb32; int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.simatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "simatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of simatcopy the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of simatcopy the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of simatcopy the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of simatcopy the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "simatcopy - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
		current_backend->extblas.simatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.simatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
	} 
	return;
}
void simatcopy(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, blasint* ldb) __attribute__((alias("simatcopy_")));


void simatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, float* alpha, float* a, int32_t* lda, int32_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	int64_t _rows64; void* _prows;
	int64_t _cols64; void* _pcols;
	int64_t _lda64; void* _plda;
	int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.simatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "simatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_rows64 = (int64_t)*rows;
		_prows = &_rows64;
		_cols64 = (int64_t)*cols;
		_pcols = &_cols64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "simatcopy32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
		current_backend->extblas.simatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.simatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
	} 
	return;
}
void simatcopy32(char* order, char* trans, int32_t* rows, int32_t* cols, float* alpha, float* a, int32_t* lda, int32_t* ldb) __attribute__((alias("simatcopy32_")));


void simatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, float* alpha, float* a, int64_t* lda, int64_t* ldb)
{
	double ts;
	void (*fn) (void* order, void* trans, void* rows, void* cols, void* alpha, void* a, void* lda, void* ldb);
	int32_t _rows32; void* _prows;
	int32_t _cols32; void* _pcols;
	int32_t _lda32; void* _plda;
	int32_t _ldb32; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.simatcopy.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "simatcopy_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_prows = rows;
		_pcols = cols;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *rows > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of simatcopy64 the parameter rows is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *cols > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of simatcopy64 the parameter cols is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of simatcopy64 the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of simatcopy64 the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_rows32 = (int32_t) *rows;
		_prows = &_rows32;
		_cols32 = (int32_t) *cols;
		_pcols = &_cols32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "simatcopy64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
		current_backend->extblas.simatcopy.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.simatcopy.calls[0]++;
	} else { 
		fn((void*) order, (void*) trans, (void*) _prows, (void*) _pcols, (void*) alpha, (void*) a, (void*) _plda, (void*) _pldb); 
	} 
	return;
}
void simatcopy64(char* order, char* trans, int64_t* rows, int64_t* cols, float* alpha, float* a, int64_t* lda, int64_t* ldb) __attribute__((alias("simatcopy64_")));



void sgeadd_(blasint* m, blasint* n, float* alpha, float* a, blasint* lda, float* beta, float* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	int32_t _m32; int64_t _m64; void* _pm;
	int32_t _n32; int64_t _n64; void* _pn;
	int32_t _lda32; int64_t _lda64; void* _plda;
	int32_t _ldb32; int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.sgeadd.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "sgeadd_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_pm = m;
		_pn = n;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *m > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of sgeadd the parameter m is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of sgeadd the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of sgeadd the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of sgeadd the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_m32 = (int32_t) *m;
		_pm = &_m32;
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_m64 = (int64_t)*m;
		_pm = &_m64;
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "sgeadd - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
		current_backend->extblas.sgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.sgeadd.calls[0]++;
	} else { 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
	} 
	return;
}
void sgeadd(blasint* m, blasint* n, float* alpha, float* a, blasint* lda, float* beta, float* b, blasint* ldb) __attribute__((alias("sgeadd_")));


void sgeadd32_(int32_t* m, int32_t* n, float* alpha, float* a, int32_t* lda, float* beta, float* b, int32_t* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	int64_t _m64; void* _pm;
	int64_t _n64; void* _pn;
	int64_t _lda64; void* _plda;
	int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.sgeadd.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "sgeadd_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_pm = m;
		_pn = n;
		_plda = lda;
		_pldb = ldb;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_m64 = (int64_t)*m;
		_pm = &_m64;
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "sgeadd32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
		current_backend->extblas.sgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.sgeadd.calls[0]++;
	} else { 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
	} 
	return;
}
void sgeadd32(int32_t* m, int32_t* n, float* alpha, float* a, int32_t* lda, float* beta, float* b, int32_t* ldb) __attribute__((alias("sgeadd32_")));


void sgeadd64_(int64_t* m, int64_t* n, float* alpha, float* a, int64_t* lda, float* beta, float* b, int64_t* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	int32_t _m32; void* _pm;
	int32_t _n32; void* _pn;
	int32_t _lda32; void* _plda;
	int32_t _ldb32; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.sgeadd.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "sgeadd_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_pm = m;
		_pn = n;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *m > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of sgeadd64 the parameter m is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of sgeadd64 the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of sgeadd64 the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of sgeadd64 the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_m32 = (int32_t) *m;
		_pm = &_m32;
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "sgeadd64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
		current_backend->extblas.sgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.sgeadd.calls[0]++;
	} else { 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
	} 
	return;
}
void sgeadd64(int64_t* m, int64_t* n, float* alpha, float* a, int64_t* lda, float* beta, float* b, int64_t* ldb) __attribute__((alias("sgeadd64_")));



void dgeadd_(blasint* m, blasint* n, double* alpha, double* a, blasint* lda, double* beta, double* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	int32_t _m32; int64_t _m64; void* _pm;
	int32_t _n32; int64_t _n64; void* _pn;
	int32_t _lda32; int64_t _lda64; void* _plda;
	int32_t _ldb32; int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.dgeadd.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "dgeadd_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_pm = m;
		_pn = n;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *m > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dgeadd the parameter m is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dgeadd the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dgeadd the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dgeadd the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_m32 = (int32_t) *m;
		_pm = &_m32;
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_m64 = (int64_t)*m;
		_pm = &_m64;
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "dgeadd - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
		current_backend->extblas.dgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.dgeadd.calls[0]++;
	} else { 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
	} 
	return;
}
void dgeadd(blasint* m, blasint* n, double* alpha, double* a, blasint* lda, double* beta, double* b, blasint* ldb) __attribute__((alias("dgeadd_")));


void dgeadd32_(int32_t* m, int32_t* n, double* alpha, double* a, int32_t* lda, double* beta, double* b, int32_t* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	int64_t _m64; void* _pm;
	int64_t _n64; void* _pn;
	int64_t _lda64; void* _plda;
	int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.dgeadd.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "dgeadd_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_pm = m;
		_pn = n;
		_plda = lda;
		_pldb = ldb;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_m64 = (int64_t)*m;
		_pm = &_m64;
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "dgeadd32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
		current_backend->extblas.dgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.dgeadd.calls[0]++;
	} else { 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
	} 
	return;
}
void dgeadd32(int32_t* m, int32_t* n, double* alpha, double* a, int32_t* lda, double* beta, double* b, int32_t* ldb) __attribute__((alias("dgeadd32_")));


void dgeadd64_(int64_t* m, int64_t* n, double* alpha, double* a, int64_t* lda, double* beta, double* b, int64_t* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	int32_t _m32; void* _pm;
	int32_t _n32; void* _pn;
	int32_t _lda32; void* _plda;
	int32_t _ldb32; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.dgeadd.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "dgeadd_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_pm = m;
		_pn = n;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *m > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dgeadd64 the parameter m is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dgeadd64 the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dgeadd64 the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of dgeadd64 the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_m32 = (int32_t) *m;
		_pm = &_m32;
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "dgeadd64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
		current_backend->extblas.dgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.dgeadd.calls[0]++;
	} else { 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
	} 
	return;
}
void dgeadd64(int64_t* m, int64_t* n, double* alpha, double* a, int64_t* lda, double* beta, double* b, int64_t* ldb) __attribute__((alias("dgeadd64_")));



void cgeadd_(blasint* m, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* beta, float complex* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	int32_t _m32; int64_t _m64; void* _pm;
	int32_t _n32; int64_t _n64; void* _pn;
	int32_t _lda32; int64_t _lda64; void* _plda;
	int32_t _ldb32; int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.cgeadd.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "cgeadd_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_pm = m;
		_pn = n;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *m > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cgeadd the parameter m is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cgeadd the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cgeadd the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cgeadd the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_m32 = (int32_t) *m;
		_pm = &_m32;
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_m64 = (int64_t)*m;
		_pm = &_m64;
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "cgeadd - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
		current_backend->extblas.cgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.cgeadd.calls[0]++;
	} else { 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
	} 
	return;
}
void cgeadd(blasint* m, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* beta, float complex* b, blasint* ldb) __attribute__((alias("cgeadd_")));


void cgeadd32_(int32_t* m, int32_t* n, float complex* alpha, float complex* a, int32_t* lda, float complex* beta, float complex* b, int32_t* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	int64_t _m64; void* _pm;
	int64_t _n64; void* _pn;
	int64_t _lda64; void* _plda;
	int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.cgeadd.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "cgeadd_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_pm = m;
		_pn = n;
		_plda = lda;
		_pldb = ldb;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_m64 = (int64_t)*m;
		_pm = &_m64;
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "cgeadd32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
		current_backend->extblas.cgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.cgeadd.calls[0]++;
	} else { 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
	} 
	return;
}
void cgeadd32(int32_t* m, int32_t* n, float complex* alpha, float complex* a, int32_t* lda, float complex* beta, float complex* b, int32_t* ldb) __attribute__((alias("cgeadd32_")));


void cgeadd64_(int64_t* m, int64_t* n, float complex* alpha, float complex* a, int64_t* lda, float complex* beta, float complex* b, int64_t* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	int32_t _m32; void* _pm;
	int32_t _n32; void* _pn;
	int32_t _lda32; void* _plda;
	int32_t _ldb32; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.cgeadd.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "cgeadd_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_pm = m;
		_pn = n;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *m > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cgeadd64 the parameter m is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cgeadd64 the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cgeadd64 the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of cgeadd64 the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_m32 = (int32_t) *m;
		_pm = &_m32;
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "cgeadd64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
		current_backend->extblas.cgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.cgeadd.calls[0]++;
	} else { 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
	} 
	return;
}
void cgeadd64(int64_t* m, int64_t* n, float complex* alpha, float complex* a, int64_t* lda, float complex* beta, float complex* b, int64_t* ldb) __attribute__((alias("cgeadd64_")));



void zgeadd_(blasint* m, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* beta, double complex* b, blasint* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	int32_t _m32; int64_t _m64; void* _pm;
	int32_t _n32; int64_t _n64; void* _pn;
	int32_t _lda32; int64_t _lda64; void* _plda;
	int32_t _ldb32; int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zgeadd.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "zgeadd_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(blasint) ) { 
		_pm = m;
		_pn = n;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(blasint) > 4 ) {
			if ( *m > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zgeadd the parameter m is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zgeadd the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zgeadd the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zgeadd the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_m32 = (int32_t) *m;
		_pm = &_m32;
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_m64 = (int64_t)*m;
		_pm = &_m64;
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "zgeadd - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
		current_backend->extblas.zgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zgeadd.calls[0]++;
	} else { 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
	} 
	return;
}
void zgeadd(blasint* m, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* beta, double complex* b, blasint* ldb) __attribute__((alias("zgeadd_")));


void zgeadd32_(int32_t* m, int32_t* n, double complex* alpha, double complex* a, int32_t* lda, double complex* beta, double complex* b, int32_t* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	int64_t _m64; void* _pm;
	int64_t _n64; void* _pn;
	int64_t _lda64; void* _plda;
	int64_t _ldb64; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zgeadd.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "zgeadd_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int32_t) ) { 
		_pm = m;
		_pn = n;
		_plda = lda;
		_pldb = ldb;
	}
	else if (current_backend->info.backend_integer_size == 8) {
		_m64 = (int64_t)*m;
		_pm = &_m64;
		_n64 = (int64_t)*n;
		_pn = &_n64;
		_lda64 = (int64_t)*lda;
		_plda = &_lda64;
		_ldb64 = (int64_t)*ldb;
		_pldb = &_ldb64;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "zgeadd32 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
		current_backend->extblas.zgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zgeadd.calls[0]++;
	} else { 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
	} 
	return;
}
void zgeadd32(int32_t* m, int32_t* n, double complex* alpha, double complex* a, int32_t* lda, double complex* beta, double complex* b, int32_t* ldb) __attribute__((alias("zgeadd32_")));


void zgeadd64_(int64_t* m, int64_t* n, double complex* alpha, double complex* a, int64_t* lda, double complex* beta, double complex* b, int64_t* ldb)
{
	double ts;
	void (*fn) (void* m, void* n, void* alpha, void* a, void* lda, void* beta, void* b, void* ldb);
	int32_t _m32; void* _pm;
	int32_t _n32; void* _pn;
	int32_t _lda32; void* _plda;
	int32_t _ldb32; void* _pldb;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->extblas.zgeadd.call_fblas; 
	if ( fn == NULL ) { 
		fprintf(stderr, PRINT_PREFIX "zgeadd_ not hooked, abort\n"); 
		abort(); 
	}
	if (current_backend->info.backend_integer_size == sizeof(int64_t) ) { 
		_pm = m;
		_pn = n;
		_plda = lda;
		_pldb = ldb;
	}
	else if ( current_backend->info.backend_integer_size == 4) {
		if ( sizeof(int64_t) > 4 ) {
			if ( *m > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zgeadd64 the parameter m is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *n > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zgeadd64 the parameter n is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *lda > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zgeadd64 the parameter lda is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
			if ( *ldb > __INT32_MAX__) {
				fprintf(stderr,"*** On entry of zgeadd64 the parameter ldb is out of int32_t range (> %d). This might cause trouble. ***\n", __INT32_MAX__);
			}
		}
		_m32 = (int32_t) *m;
		_pm = &_m32;
		_n32 = (int32_t) *n;
		_pn = &_n32;
		_lda32 = (int32_t) *lda;
		_plda = &_lda32;
		_ldb32 = (int32_t) *ldb;
		_pldb = &_ldb32;
	}
	else {
		fprintf(stderr, PRINT_PREFIX "zgeadd64 - can not convert integer types");
		abort();
	}

	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
		current_backend->extblas.zgeadd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->extblas.zgeadd.calls[0]++;
	} else { 
		fn((void*) _pm, (void*) _pn, (void*) alpha, (void*) a, (void*) _plda, (void*) beta, (void*) b, (void*) _pldb); 
	} 
	return;
}
void zgeadd64(int64_t* m, int64_t* n, double complex* alpha, double complex* a, int64_t* lda, double complex* beta, double complex* b, int64_t* ldb) __attribute__((alias("zgeadd64_")));



