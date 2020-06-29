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



static TLS_STORE uint8_t hook_pos_clabrd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clabrd,CLABRD)(blasint* m, blasint* n, blasint* nb, float complex* a, blasint* lda, float* d, float* e, float complex* tauq, float complex* taup, float complex* x, blasint* ldx, float complex* y, blasint* ldy)
#else
void FC_GLOBAL(clabrd,CLABRD)(blasint* m, blasint* n, blasint* nb, float complex* a, blasint* lda, float* d, float* e, float complex* tauq, float complex* taup, float complex* x, blasint* ldx, float complex* y, blasint* ldy)
#endif
{
	void (*fn) (void* m, void* n, void* nb, void* a, void* lda, void* d, void* e, void* tauq, void* taup, void* x, void* ldx, void* y, void* ldy);
	void (*fn_hook) (void* m, void* n, void* nb, void* a, void* lda, void* d, void* e, void* tauq, void* taup, void* x, void* ldx, void* y, void* ldy);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.clabrd.f77_blas_function; 
	fn_hook = __flexiblas_hooks->clabrd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m, (void*) n, (void*) nb, (void*) a, (void*) lda, (void*) d, (void*) e, (void*) tauq, (void*) taup, (void*) x, (void*) ldx, (void*) y, (void*) ldy); 
		return;
	} else {
		hook_pos_clabrd = 0;
		fn_hook((void*) m, (void*) n, (void*) nb, (void*) a, (void*) lda, (void*) d, (void*) e, (void*) tauq, (void*) taup, (void*) x, (void*) ldx, (void*) y, (void*) ldy);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clabrd_(blasint* m, blasint* n, blasint* nb, float complex* a, blasint* lda, float* d, float* e, float complex* tauq, float complex* taup, float complex* x, blasint* ldx, float complex* y, blasint* ldy) __attribute__((alias(MTS(FC_GLOBAL(clabrd,CLABRD)))));
#else
void clabrd(blasint* m, blasint* n, blasint* nb, float complex* a, blasint* lda, float* d, float* e, float complex* tauq, float complex* taup, float complex* x, blasint* ldx, float complex* y, blasint* ldy) __attribute__((alias(MTS(FC_GLOBAL(clabrd,CLABRD)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clabrd_(void* m, void* n, void* nb, void* a, void* lda, void* d, void* e, void* tauq, void* taup, void* x, void* ldx, void* y, void* ldy)
{
	void (*fn) (void* m, void* n, void* nb, void* a, void* lda, void* d, void* e, void* tauq, void* taup, void* x, void* ldx, void* y, void* ldy);

	fn = current_backend->lapack.clabrd.f77_blas_function; 

		fn((void*) m, (void*) n, (void*) nb, (void*) a, (void*) lda, (void*) d, (void*) e, (void*) tauq, (void*) taup, (void*) x, (void*) ldx, (void*) y, (void*) ldy); 

	return;
}

void flexiblas_real_clabrd(void* m, void* n, void* nb, void* a, void* lda, void* d, void* e, void* tauq, void* taup, void* x, void* ldx, void* y, void* ldy)  __attribute__((alias("flexiblas_real_clabrd_")));





/* Chainloader for Hooks */


void flexiblas_chain_clabrd_(void* m, void* n, void* nb, void* a, void* lda, void* d, void* e, void* tauq, void* taup, void* x, void* ldx, void* y, void* ldy)
{
	void (*fn) (void* m, void* n, void* nb, void* a, void* lda, void* d, void* e, void* tauq, void* taup, void* x, void* ldx, void* y, void* ldy);
	void (*fn_hook) (void* m, void* n, void* nb, void* a, void* lda, void* d, void* e, void* tauq, void* taup, void* x, void* ldx, void* y, void* ldy);

	fn      = current_backend->lapack.clabrd.f77_blas_function; 

    hook_pos_clabrd ++;
    if( hook_pos_clabrd < __flexiblas_hooks->clabrd.nhook) {
        fn_hook = __flexiblas_hooks->clabrd.f77_hook_function[hook_pos_clabrd];
        fn_hook((void*) m, (void*) n, (void*) nb, (void*) a, (void*) lda, (void*) d, (void*) e, (void*) tauq, (void*) taup, (void*) x, (void*) ldx, (void*) y, (void*) ldy);
    } else {
        hook_pos_clabrd = 0;
		fn((void*) m, (void*) n, (void*) nb, (void*) a, (void*) lda, (void*) d, (void*) e, (void*) tauq, (void*) taup, (void*) x, (void*) ldx, (void*) y, (void*) ldy); 
	}
	return;
}

void flexiblas_chain_clabrd(void* m, void* n, void* nb, void* a, void* lda, void* d, void* e, void* tauq, void* taup, void* x, void* ldx, void* y, void* ldy)  __attribute__((alias("flexiblas_chain_clabrd_")));




