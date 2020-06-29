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
 * Copyright (C) Martin Koehler, 2013-2020
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Wed Mar 28 11:20:03 2018 */
        
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



static TLS_STORE uint8_t hook_pos_dbbcsd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dbbcsd,DBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double* u1, blasint* ldu1, double* u2, blasint* ldu2, double* v1t, blasint* ldv1t, double* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL(dbbcsd,DBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double* u1, blasint* ldu1, double* u2, blasint* ldu2, double* v1t, blasint* ldv1t, double* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* work, blasint* lwork, blasint* info)
#endif
{
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info);
	void (*fn_hook) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dbbcsd.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dbbcsd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info); 
		return;
	} else {
		hook_pos_dbbcsd = 0;
		fn_hook((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dbbcsd_(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double* u1, blasint* ldu1, double* u2, blasint* ldu2, double* v1t, blasint* ldv1t, double* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dbbcsd,DBBCSD)))));
#else
void dbbcsd(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double* u1, blasint* ldu1, double* u2, blasint* ldu2, double* v1t, blasint* ldv1t, double* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dbbcsd,DBBCSD)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dbbcsd_(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info)
{
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info);

	fn = current_backend->lapack.dbbcsd.f77_blas_function; 

		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info); 

	return;
}

void flexiblas_real_dbbcsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info)  __attribute__((alias("flexiblas_real_dbbcsd_")));





/* Chainloader for Hooks */


void flexiblas_chain_dbbcsd_(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info)
{
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info);
	void (*fn_hook) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info);

	fn      = current_backend->lapack.dbbcsd.f77_blas_function; 

    hook_pos_dbbcsd ++;
    if( hook_pos_dbbcsd < __flexiblas_hooks->dbbcsd.nhook) {
        fn_hook = __flexiblas_hooks->dbbcsd.f77_hook_function[hook_pos_dbbcsd];
        fn_hook((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info);
    } else {
        hook_pos_dbbcsd = 0;
		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) work, (void*) lwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_dbbcsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* work, void* lwork, void* info)  __attribute__((alias("flexiblas_chain_dbbcsd_")));




