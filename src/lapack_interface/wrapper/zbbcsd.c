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
 /* Generated: Tue Mar 28 16:07:37 2017 */ 
        
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
void FC_GLOBAL(zbbcsd,ZBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double complex* u1, blasint* ldu1, double complex* u2, blasint* ldu2, double complex* v1t, blasint* ldv1t, double complex* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* rwork, blasint* lrwork, blasint* info)
#else
void FC_GLOBAL(zbbcsd,ZBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double complex* u1, blasint* ldu1, double complex* u2, blasint* ldu2, double complex* v1t, blasint* ldv1t, double complex* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* rwork, blasint* lrwork, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* rwork, void* lrwork, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.zbbcsd.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) rwork, (void*) lrwork, (void*) info); 
		current_backend->lapack.zbbcsd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.zbbcsd.calls[0]++;
	} else { 
		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) rwork, (void*) lrwork, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void zbbcsd_(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double complex* u1, blasint* ldu1, double complex* u2, blasint* ldu2, double complex* v1t, blasint* ldv1t, double complex* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* rwork, blasint* lrwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zbbcsd,ZBBCSD)))));
#else
void zbbcsd(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double complex* u1, blasint* ldu1, double complex* u2, blasint* ldu2, double complex* v1t, blasint* ldv1t, double complex* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* rwork, blasint* lrwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zbbcsd,ZBBCSD)))));
#endif



