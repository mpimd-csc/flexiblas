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
 /* Generated: Tue Mar 28 16:07:33 2017 */ 
        
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
void FC_GLOBAL(clar1v,CLAR1V)(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float complex* z, blasint* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work)
#else
void FC_GLOBAL(clar1v,CLAR1V)(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float complex* z, blasint* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work)
#endif 
{
    double ts;
	void (*fn) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.clar1v.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work); 
		current_backend->lapack.clar1v.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.clar1v.calls[0]++;
	} else { 
		fn((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void clar1v_(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float complex* z, blasint* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work) __attribute__((alias(MTS(FC_GLOBAL(clar1v,CLAR1V)))));
#else
void clar1v(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float complex* z, blasint* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work) __attribute__((alias(MTS(FC_GLOBAL(clar1v,CLAR1V)))));
#endif



