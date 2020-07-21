/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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
 * Linking FlexiBLAS statically or dynamically with other modules is making a combined
 * work based on FlexiBLAS. Thus, the terms and conditions of the GNU General
 * Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of FlexiBLAS give you permission
 * to combine FlexiBLAS program with free software programs or libraries that are
 * released under the GNU LGPL and with independent modules that communicate with
 * FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
 * BLAS/LAPACK reference implementation. You may copy and distribute such a system
 * following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
 * code concerned, provided that you include the source code of that other code
 * when and as the GNU GPL requires distribution of source code and provided that
 * you do not modify the BLAS/LAPACK interface.
 *
 * Note that people who make modified versions of FlexiBLAS are not obligated to
 * grant this special exception for their modified versions; it is their choice
 * whether to do so. The GNU General Public License gives permission to release a
 * modified version without this exception; this exception also makes it possible
 * to release a modified version which carries forward this exception. If you
 * modify the BLAS/LAPACK interface, this exception does not apply to your
 * modified version of FlexiBLAS, and you must remove this exception when you
 * distribute your modified version.
 *
 * This exception is an additional permission under section 7 of the GNU General
 * Public License, version 3 (“GPLv3”)
 *
 *
 * Copyright (C) Martin Koehler, 2015-2017
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Fri Jul 10 16:25:22 2020 */
        
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



static TLS_STORE uint8_t hook_pos_claqr5 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claqr5,CLAQR5)(blasint* wantt, blasint* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, float complex* s, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, float complex* v, blasint* ldv, float complex* u, blasint* ldu, blasint* nv, float complex* wv, blasint* ldwv, blasint* nh, float complex* wh, blasint* ldwh)
#else
void FC_GLOBAL(claqr5,CLAQR5)(blasint* wantt, blasint* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, float complex* s, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, float complex* v, blasint* ldv, float complex* u, blasint* ldu, blasint* nv, float complex* wv, blasint* ldwv, blasint* nh, float complex* wh, blasint* ldwh)
#endif
{
	void (*fn) (void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh);
	void (*fn_hook) (void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.claqr5.f77_blas_function; 
	fn_hook = __flexiblas_hooks->claqr5.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh); 
		return;
	} else {
		hook_pos_claqr5 = 0;
		fn_hook((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void claqr5_(blasint* wantt, blasint* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, float complex* s, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, float complex* v, blasint* ldv, float complex* u, blasint* ldu, blasint* nv, float complex* wv, blasint* ldwv, blasint* nh, float complex* wh, blasint* ldwh) __attribute__((alias(MTS(FC_GLOBAL(claqr5,CLAQR5)))));
#else
void claqr5(blasint* wantt, blasint* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, float complex* s, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, float complex* v, blasint* ldv, float complex* u, blasint* ldu, blasint* nv, float complex* wv, blasint* ldwv, blasint* nh, float complex* wh, blasint* ldwh) __attribute__((alias(MTS(FC_GLOBAL(claqr5,CLAQR5)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claqr5_(void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh)
{
	void (*fn) (void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh);

	fn = current_backend->lapack.claqr5.f77_blas_function; 

		fn((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh); 

	return;
}

void flexiblas_real_claqr5(void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh)  __attribute__((alias("flexiblas_real_claqr5_")));





/* Chainloader for Hooks */


void flexiblas_chain_claqr5_(void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh)
{
	void (*fn) (void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh);
	void (*fn_hook) (void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh);

	fn      = current_backend->lapack.claqr5.f77_blas_function; 

    hook_pos_claqr5 ++;
    if( hook_pos_claqr5 < __flexiblas_hooks->claqr5.nhook) {
        fn_hook = __flexiblas_hooks->claqr5.f77_hook_function[hook_pos_claqr5];
        fn_hook((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh);
    } else {
        hook_pos_claqr5 = 0;
		fn((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh); 
	}
	return;
}

void flexiblas_chain_claqr5(void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh)  __attribute__((alias("flexiblas_chain_claqr5_")));




