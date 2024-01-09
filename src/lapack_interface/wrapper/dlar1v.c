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


#if __GNUC__ > 7
typedef size_t fortran_charlen_t;
#else
typedef int fortran_charlen_t;
#endif

#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_dlar1v = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlar1v,DLAR1V)(blasint* n, blasint* b1, blasint* bn, double* lambda, double* d, double* l, double* ld, double* lld, double* pivmin, double* gaptol, double* z, blasint* wantnc, blasint* negcnt, double* ztz, double* mingma, blasint* r, blasint* isuppz, double* nrminv, double* resid, double* rqcorr, double* work)
#else
void FC_GLOBAL(dlar1v,DLAR1V)(blasint* n, blasint* b1, blasint* bn, double* lambda, double* d, double* l, double* ld, double* lld, double* pivmin, double* gaptol, double* z, blasint* wantnc, blasint* negcnt, double* ztz, double* mingma, blasint* r, blasint* isuppz, double* nrminv, double* resid, double* rqcorr, double* work)
#endif
{
	void (*fn) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);
	void (*fn_hook) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlar1v.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlar1v.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work); 
		return;
	} else {
		hook_pos_dlar1v = 0;
		fn_hook((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlar1v_(blasint* n, blasint* b1, blasint* bn, double* lambda, double* d, double* l, double* ld, double* lld, double* pivmin, double* gaptol, double* z, blasint* wantnc, blasint* negcnt, double* ztz, double* mingma, blasint* r, blasint* isuppz, double* nrminv, double* resid, double* rqcorr, double* work) __attribute__((alias(MTS(FC_GLOBAL(dlar1v,DLAR1V)))));
#else
#ifndef __APPLE__
void dlar1v(blasint* n, blasint* b1, blasint* bn, double* lambda, double* d, double* l, double* ld, double* lld, double* pivmin, double* gaptol, double* z, blasint* wantnc, blasint* negcnt, double* ztz, double* mingma, blasint* r, blasint* isuppz, double* nrminv, double* resid, double* rqcorr, double* work) __attribute__((alias(MTS(FC_GLOBAL(dlar1v,DLAR1V)))));
#else
void dlar1v(blasint* n, blasint* b1, blasint* bn, double* lambda, double* d, double* l, double* ld, double* lld, double* pivmin, double* gaptol, double* z, blasint* wantnc, blasint* negcnt, double* ztz, double* mingma, blasint* r, blasint* isuppz, double* nrminv, double* resid, double* rqcorr, double* work){ FC_GLOBAL(dlar1v,DLAR1V)((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlar1v_(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work)
{
	void (*fn) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);

	*(void **) & fn = current_backend->lapack.dlar1v.f77_blas_function; 

		fn((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlar1v(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work) __attribute__((alias("flexiblas_real_dlar1v_")));
#else
void flexiblas_real_dlar1v(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work){flexiblas_real_dlar1v_((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlar1v_(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work)
{
	void (*fn) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);
	void (*fn_hook) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);

	*(void **) &fn      = current_backend->lapack.dlar1v.f77_blas_function; 

    hook_pos_dlar1v ++;
    if( hook_pos_dlar1v < __flexiblas_hooks->dlar1v.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlar1v.f77_hook_function[hook_pos_dlar1v];
        fn_hook((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);
    } else {
        hook_pos_dlar1v = 0;
		fn((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlar1v(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work) __attribute__((alias("flexiblas_chain_dlar1v_")));
#else
void flexiblas_chain_dlar1v(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work){flexiblas_chain_dlar1v_((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);}
#endif



