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
 /* Generated: Tue Mar 28 16:07:34 2017 */ 
        
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
void FC_GLOBAL(dlaqr2,DLAQR2)(blasint* wantt, blasint* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, double* h, blasint* ldh, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, blasint* ns, blasint* nd, double* sr, double* si, double* v, blasint* ldv, blasint* nh, double* t, blasint* ldt, blasint* nv, double* wv, blasint* ldwv, double* work, blasint* lwork)
#else
void FC_GLOBAL(dlaqr2,DLAQR2)(blasint* wantt, blasint* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, double* h, blasint* ldh, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, blasint* ns, blasint* nd, double* sr, double* si, double* v, blasint* ldv, blasint* nh, double* t, blasint* ldt, blasint* nv, double* wv, blasint* ldwv, double* work, blasint* lwork)
#endif 
{
    double ts;
	void (*fn) (void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sr, void* si, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.dlaqr2.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sr, (void*) si, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork); 
		current_backend->lapack.dlaqr2.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.dlaqr2.calls[0]++;
	} else { 
		fn((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sr, (void*) si, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void dlaqr2_(blasint* wantt, blasint* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, double* h, blasint* ldh, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, blasint* ns, blasint* nd, double* sr, double* si, double* v, blasint* ldv, blasint* nh, double* t, blasint* ldt, blasint* nv, double* wv, blasint* ldwv, double* work, blasint* lwork) __attribute__((alias(MTS(FC_GLOBAL(dlaqr2,DLAQR2)))));
#else
void dlaqr2(blasint* wantt, blasint* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, double* h, blasint* ldh, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, blasint* ns, blasint* nd, double* sr, double* si, double* v, blasint* ldv, blasint* nh, double* t, blasint* ldt, blasint* nv, double* wv, blasint* ldwv, double* work, blasint* lwork) __attribute__((alias(MTS(FC_GLOBAL(dlaqr2,DLAQR2)))));
#endif



