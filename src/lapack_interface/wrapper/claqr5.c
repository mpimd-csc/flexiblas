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
void FC_GLOBAL(claqr5,CLAQR5)(blasint* wantt, blasint* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, float complex* s, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, float complex* v, blasint* ldv, float complex* u, blasint* ldu, blasint* nv, float complex* wv, blasint* ldwv, blasint* nh, float complex* wh, blasint* ldwh)
#else
void FC_GLOBAL(claqr5,CLAQR5)(blasint* wantt, blasint* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, float complex* s, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, float complex* v, blasint* ldv, float complex* u, blasint* ldu, blasint* nv, float complex* wv, blasint* ldwv, blasint* nh, float complex* wh, blasint* ldwh)
#endif 
{
    double ts;
	void (*fn) (void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.claqr5.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh); 
		current_backend->lapack.claqr5.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.claqr5.calls[0]++;
	} else { 
		fn((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void claqr5_(blasint* wantt, blasint* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, float complex* s, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, float complex* v, blasint* ldv, float complex* u, blasint* ldu, blasint* nv, float complex* wv, blasint* ldwv, blasint* nh, float complex* wh, blasint* ldwh) __attribute__((alias(MTS(FC_GLOBAL(claqr5,CLAQR5)))));
#else
void claqr5(blasint* wantt, blasint* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, float complex* s, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, float complex* v, blasint* ldv, float complex* u, blasint* ldu, blasint* nv, float complex* wv, blasint* ldwv, blasint* nh, float complex* wh, blasint* ldwh) __attribute__((alias(MTS(FC_GLOBAL(claqr5,CLAQR5)))));
#endif



