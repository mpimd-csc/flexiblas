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
void FC_GLOBAL(cgbbrd,CGBBRD)(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, float complex* ab, blasint* ldab, float* d, float* e, float complex* q, blasint* ldq, float complex* pt, blasint* ldpt, float complex* c, blasint* ldc, float complex* work, float* rwork, blasint* info)
#else
void FC_GLOBAL(cgbbrd,CGBBRD)(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, float complex* ab, blasint* ldab, float* d, float* e, float complex* q, blasint* ldq, float complex* pt, blasint* ldpt, float complex* c, blasint* ldc, float complex* work, float* rwork, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.cgbbrd.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info); 
		current_backend->lapack.cgbbrd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.cgbbrd.calls[0]++;
	} else { 
		fn((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void cgbbrd_(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, float complex* ab, blasint* ldab, float* d, float* e, float complex* q, blasint* ldq, float complex* pt, blasint* ldpt, float complex* c, blasint* ldc, float complex* work, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgbbrd,CGBBRD)))));
#else
void cgbbrd(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, float complex* ab, blasint* ldab, float* d, float* e, float complex* q, blasint* ldq, float complex* pt, blasint* ldpt, float complex* c, blasint* ldc, float complex* work, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgbbrd,CGBBRD)))));
#endif



