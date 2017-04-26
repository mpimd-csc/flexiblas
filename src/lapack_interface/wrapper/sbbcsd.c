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
void FC_GLOBAL(sbbcsd,SBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, float* theta, float* phi, float* u1, blasint* ldu1, float* u2, blasint* ldu2, float* v1t, blasint* ldv1t, float* v2t, blasint* ldv2t, float* b11d, float* b11e, float* b12d, float* b12e, float* b21d, float* b21e, float* b22d, float* b22e, float* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL(sbbcsd,SBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, float* theta, float* phi, float* u1, blasint* ldu1, float* u2, blasint* ldu2, float* v1t, blasint* ldv1t, float* v2t, blasint* ldv2t, float* b11d, float* b11e, float* b12d, float* b12e, float* b21d, float* b21e, float* b22d, float* b22e, float* work, blasint* lwork, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.sbbcsd.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info); 
		current_backend->lapack.sbbcsd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.sbbcsd.calls[0]++;
	} else { 
		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void sbbcsd_(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, float* theta, float* phi, float* u1, blasint* ldu1, float* u2, blasint* ldu2, float* v1t, blasint* ldv1t, float* v2t, blasint* ldv2t, float* b11d, float* b11e, float* b12d, float* b12e, float* b21d, float* b21e, float* b22d, float* b22e, float* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sbbcsd,SBBCSD)))));
#else
void sbbcsd(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, float* theta, float* phi, float* u1, blasint* ldu1, float* u2, blasint* ldu2, float* v1t, blasint* ldv1t, float* v2t, blasint* ldv2t, float* b11d, float* b11e, float* b12d, float* b12e, float* b21d, float* b21e, float* b22d, float* b22e, float* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sbbcsd,SBBCSD)))));
#endif



