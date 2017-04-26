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
 * Copyright (C) Martin Koehler, 2015-2017
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Tue Mar 28 16:07:32 2017 */ 
        
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



#ifdef FLEXIBLAS_ABI_INTEL 
void FC_GLOBAL(cgtrfs,CGTRFS)(char* trans, blasint* n, blasint* nrhs, float complex* dl, float complex* d, float complex* du, float complex* dlf, float complex* df, float complex* duf, float complex* du2, blasint* ipiv, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info)
#else
void FC_GLOBAL(cgtrfs,CGTRFS)(char* trans, blasint* n, blasint* nrhs, float complex* dl, float complex* d, float complex* du, float complex* dlf, float complex* df, float complex* duf, float complex* du2, blasint* ipiv, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* trans, void* n, void* nrhs, void* dl, void* d, void* du, void* dlf, void* df, void* duf, void* du2, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.cgtrfs.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) trans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) dlf, (void*) df, (void*) duf, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info); 
		current_backend->lapack.cgtrfs.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.cgtrfs.calls[0]++;
	} else { 
		fn((void*) trans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) dlf, (void*) df, (void*) duf, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void cgtrfs_(char* trans, blasint* n, blasint* nrhs, float complex* dl, float complex* d, float complex* du, float complex* dlf, float complex* df, float complex* duf, float complex* du2, blasint* ipiv, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgtrfs,CGTRFS)))));
#else
void cgtrfs(char* trans, blasint* n, blasint* nrhs, float complex* dl, float complex* d, float complex* du, float complex* dlf, float complex* df, float complex* duf, float complex* du2, blasint* ipiv, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgtrfs,CGTRFS)))));
#endif



