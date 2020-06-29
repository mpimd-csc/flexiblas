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



static TLS_STORE uint8_t hook_pos_claqr2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claqr2,CLAQR2)(blasint* wantt, blasint* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, blasint* ns, blasint* nd, float complex* sh, float complex* v, blasint* ldv, blasint* nh, float complex* t, blasint* ldt, blasint* nv, float complex* wv, blasint* ldwv, float complex* work, blasint* lwork)
#else
void FC_GLOBAL(claqr2,CLAQR2)(blasint* wantt, blasint* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, blasint* ns, blasint* nd, float complex* sh, float complex* v, blasint* ldv, blasint* nh, float complex* t, blasint* ldt, blasint* nv, float complex* wv, blasint* ldwv, float complex* work, blasint* lwork)
#endif
{
	void (*fn) (void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork);
	void (*fn_hook) (void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.claqr2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->claqr2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork); 
		return;
	} else {
		hook_pos_claqr2 = 0;
		fn_hook((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void claqr2_(blasint* wantt, blasint* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, blasint* ns, blasint* nd, float complex* sh, float complex* v, blasint* ldv, blasint* nh, float complex* t, blasint* ldt, blasint* nv, float complex* wv, blasint* ldwv, float complex* work, blasint* lwork) __attribute__((alias(MTS(FC_GLOBAL(claqr2,CLAQR2)))));
#else
void claqr2(blasint* wantt, blasint* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, blasint* ns, blasint* nd, float complex* sh, float complex* v, blasint* ldv, blasint* nh, float complex* t, blasint* ldt, blasint* nv, float complex* wv, blasint* ldwv, float complex* work, blasint* lwork) __attribute__((alias(MTS(FC_GLOBAL(claqr2,CLAQR2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claqr2_(void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork)
{
	void (*fn) (void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork);

	fn = current_backend->lapack.claqr2.f77_blas_function; 

		fn((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork); 

	return;
}

void flexiblas_real_claqr2(void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork)  __attribute__((alias("flexiblas_real_claqr2_")));





/* Chainloader for Hooks */


void flexiblas_chain_claqr2_(void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork)
{
	void (*fn) (void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork);
	void (*fn_hook) (void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork);

	fn      = current_backend->lapack.claqr2.f77_blas_function; 

    hook_pos_claqr2 ++;
    if( hook_pos_claqr2 < __flexiblas_hooks->claqr2.nhook) {
        fn_hook = __flexiblas_hooks->claqr2.f77_hook_function[hook_pos_claqr2];
        fn_hook((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork);
    } else {
        hook_pos_claqr2 = 0;
		fn((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork); 
	}
	return;
}

void flexiblas_chain_claqr2(void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork)  __attribute__((alias("flexiblas_chain_claqr2_")));




