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
 /* Generated: Tue Mar 28 16:07:35 2017 */ 
        
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
void FC_GLOBAL(dstevr,DSTEVR)(char* jobz, char* range, blasint* n, double* d, double* e, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info)
#else
void FC_GLOBAL(dstevr,DSTEVR)(char* jobz, char* range, blasint* n, double* d, double* e, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* jobz, void* range, void* n, void* d, void* e, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* isuppz, void* work, void* lwork, void* iwork, void* liwork, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.dstevr.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) jobz, (void*) range, (void*) n, (void*) d, (void*) e, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) m, (void*) w, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); 
		current_backend->lapack.dstevr.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.dstevr.calls[0]++;
	} else { 
		fn((void*) jobz, (void*) range, (void*) n, (void*) d, (void*) e, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) m, (void*) w, (void*) z, (void*) ldz, (void*) isuppz, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void dstevr_(char* jobz, char* range, blasint* n, double* d, double* e, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dstevr,DSTEVR)))));
#else
void dstevr(char* jobz, char* range, blasint* n, double* d, double* e, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dstevr,DSTEVR)))));
#endif



