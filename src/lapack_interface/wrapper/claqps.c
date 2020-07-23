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
 * Copyright (C) Martin Koehler, 2015-2020
 */
        
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



static TLS_STORE uint8_t hook_pos_claqps = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claqps,CLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float* vn1, float* vn2, float complex* auxv, float complex* f, blasint* ldf)
#else
void FC_GLOBAL(claqps,CLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float* vn1, float* vn2, float complex* auxv, float complex* f, blasint* ldf)
#endif
{
	void (*fn) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);
	void (*fn_hook) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.claqps.f77_blas_function; 
	fn_hook = __flexiblas_hooks->claqps.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf); 
		return;
	} else {
		hook_pos_claqps = 0;
		fn_hook((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void claqps_(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float* vn1, float* vn2, float complex* auxv, float complex* f, blasint* ldf) __attribute__((alias(MTS(FC_GLOBAL(claqps,CLAQPS)))));
#else
void claqps(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float* vn1, float* vn2, float complex* auxv, float complex* f, blasint* ldf) __attribute__((alias(MTS(FC_GLOBAL(claqps,CLAQPS)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claqps_(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf)
{
	void (*fn) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);

	fn = current_backend->lapack.claqps.f77_blas_function; 

		fn((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf); 

	return;
}

void flexiblas_real_claqps(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf)  __attribute__((alias("flexiblas_real_claqps_")));





/* Chainloader for Hooks */


void flexiblas_chain_claqps_(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf)
{
	void (*fn) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);
	void (*fn_hook) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);

	fn      = current_backend->lapack.claqps.f77_blas_function; 

    hook_pos_claqps ++;
    if( hook_pos_claqps < __flexiblas_hooks->claqps.nhook) {
        fn_hook = __flexiblas_hooks->claqps.f77_hook_function[hook_pos_claqps];
        fn_hook((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);
    } else {
        hook_pos_claqps = 0;
		fn((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf); 
	}
	return;
}

void flexiblas_chain_claqps(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf)  __attribute__((alias("flexiblas_chain_claqps_")));




