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
 /* Generated: Wed Mar 28 11:20:04 2018 */
        
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



static TLS_STORE uint8_t hook_pos_slar1v = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slar1v,SLAR1V)(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float* z, blasint* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work)
#else
void FC_GLOBAL(slar1v,SLAR1V)(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float* z, blasint* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work)
#endif
{
	void (*fn) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);
	void (*fn_hook) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slar1v.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slar1v.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work); 
		return;
	} else {
		hook_pos_slar1v = 0;
		fn_hook((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slar1v_(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float* z, blasint* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work) __attribute__((alias(MTS(FC_GLOBAL(slar1v,SLAR1V)))));
#else
void slar1v(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float* z, blasint* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work) __attribute__((alias(MTS(FC_GLOBAL(slar1v,SLAR1V)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slar1v_(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work)
{
	void (*fn) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);

	fn = current_backend->lapack.slar1v.f77_blas_function; 

		fn((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work); 

	return;
}

void flexiblas_real_slar1v(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work)  __attribute__((alias("flexiblas_real_slar1v_")));





/* Chainloader for Hooks */


void flexiblas_chain_slar1v_(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work)
{
	void (*fn) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);
	void (*fn_hook) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);

	fn      = current_backend->lapack.slar1v.f77_blas_function; 

    hook_pos_slar1v ++;
    if( hook_pos_slar1v < __flexiblas_hooks->slar1v.nhook) {
        fn_hook = __flexiblas_hooks->slar1v.f77_hook_function[hook_pos_slar1v];
        fn_hook((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);
    } else {
        hook_pos_slar1v = 0;
		fn((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work); 
	}
	return;
}

void flexiblas_chain_slar1v(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work)  __attribute__((alias("flexiblas_chain_slar1v_")));




