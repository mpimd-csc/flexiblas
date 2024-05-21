//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program. If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_cbbcsd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cbbcsd,CBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, float* theta, float* phi, float complex* u1, blasint* ldu1, float complex* u2, blasint* ldu2, float complex* v1t, blasint* ldv1t, float complex* v2t, blasint* ldv2t, float* b11d, float* b11e, float* b12d, float* b12e, float* b21d, float* b21e, float* b22d, float* b22e, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans)
#else
void FC_GLOBAL(cbbcsd,CBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, float* theta, float* phi, float complex* u1, blasint* ldu1, float complex* u2, blasint* ldu2, float complex* v1t, blasint* ldv1t, float complex* v2t, blasint* ldv2t, float* b11d, float* b11e, float* b12d, float* b12e, float* b21d, float* b21e, float* b22d, float* b22e, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans)
#endif
{
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);
	void (*fn_hook) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.cbbcsd.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->cbbcsd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans); 
		return;
	} else {
		hook_pos_cbbcsd = 0;
		fn_hook((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cbbcsd_(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, float* theta, float* phi, float complex* u1, blasint* ldu1, float complex* u2, blasint* ldu2, float complex* v1t, blasint* ldv1t, float complex* v2t, blasint* ldv2t, float* b11d, float* b11e, float* b12d, float* b12e, float* b21d, float* b21e, float* b22d, float* b22e, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(cbbcsd,CBBCSD)))));
#else
#ifndef __APPLE__
void cbbcsd(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, float* theta, float* phi, float complex* u1, blasint* ldu1, float complex* u2, blasint* ldu2, float complex* v1t, blasint* ldv1t, float complex* v2t, blasint* ldv2t, float* b11d, float* b11e, float* b12d, float* b12e, float* b21d, float* b21e, float* b22d, float* b22e, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(cbbcsd,CBBCSD)))));
#else
void cbbcsd(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, float* theta, float* phi, float complex* u1, blasint* ldu1, float complex* u2, blasint* ldu2, float complex* v1t, blasint* ldv1t, float complex* v2t, blasint* ldv2t, float* b11d, float* b11e, float* b12d, float* b12e, float* b21d, float* b21e, float* b22d, float* b22e, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(cbbcsd,CBBCSD)((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu1, (flexiblas_fortran_charlen_t) len_jobu2, (flexiblas_fortran_charlen_t) len_jobv1t, (flexiblas_fortran_charlen_t) len_jobv2t, (flexiblas_fortran_charlen_t) len_trans); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cbbcsd_(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans)
{
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);

	*(void **) & fn = current_backend->lapack.cbbcsd.f77_blas_function; 

		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_cbbcsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_cbbcsd_")));
#else
void flexiblas_real_cbbcsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans){flexiblas_real_cbbcsd_((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu1, (flexiblas_fortran_charlen_t) len_jobu2, (flexiblas_fortran_charlen_t) len_jobv1t, (flexiblas_fortran_charlen_t) len_jobv2t, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cbbcsd_(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans)
{
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);
	void (*fn_hook) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);

	*(void **) &fn      = current_backend->lapack.cbbcsd.f77_blas_function; 

    hook_pos_cbbcsd ++;
    if( hook_pos_cbbcsd < __flexiblas_hooks->cbbcsd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cbbcsd.f77_hook_function[hook_pos_cbbcsd];
        fn_hook((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans);
    } else {
        hook_pos_cbbcsd = 0;
		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_cbbcsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_cbbcsd_")));
#else
void flexiblas_chain_cbbcsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* m, void* p, void* q, void* theta, void* phi, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* b11d, void* b11e, void* b12d, void* b12e, void* b21d, void* b21e, void* b22d, void* b22e, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans){flexiblas_chain_cbbcsd_((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) m, (void*) p, (void*) q, (void*) theta, (void*) phi, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) b11d, (void*) b11e, (void*) b12d, (void*) b12e, (void*) b21d, (void*) b21e, (void*) b22d, (void*) b22e, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu1, (flexiblas_fortran_charlen_t) len_jobu2, (flexiblas_fortran_charlen_t) len_jobv1t, (flexiblas_fortran_charlen_t) len_jobv2t, (flexiblas_fortran_charlen_t) len_trans);}
#endif



