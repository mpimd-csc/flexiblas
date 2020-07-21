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
 /* Generated: Fri Jul 10 16:25:23 2020 */
        
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



static TLS_STORE uint8_t hook_pos_slasd7 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasd7,SLASD7)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* zw, float* vf, float* vfw, float* vl, float* vlw, float* alpha, float* beta, float* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* c, float* s, blasint* info)
#else
void FC_GLOBAL(slasd7,SLASD7)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* zw, float* vf, float* vfw, float* vl, float* vlw, float* alpha, float* beta, float* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* c, float* s, blasint* info)
#endif
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);
	void (*fn_hook) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slasd7.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slasd7.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info); 
		return;
	} else {
		hook_pos_slasd7 = 0;
		fn_hook((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slasd7_(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* zw, float* vf, float* vfw, float* vl, float* vlw, float* alpha, float* beta, float* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* c, float* s, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slasd7,SLASD7)))));
#else
void slasd7(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* zw, float* vf, float* vfw, float* vl, float* vlw, float* alpha, float* beta, float* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* c, float* s, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slasd7,SLASD7)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasd7_(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info)
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);

	fn = current_backend->lapack.slasd7.f77_blas_function; 

		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info); 

	return;
}

void flexiblas_real_slasd7(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info)  __attribute__((alias("flexiblas_real_slasd7_")));





/* Chainloader for Hooks */


void flexiblas_chain_slasd7_(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info)
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);
	void (*fn_hook) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);

	fn      = current_backend->lapack.slasd7.f77_blas_function; 

    hook_pos_slasd7 ++;
    if( hook_pos_slasd7 < __flexiblas_hooks->slasd7.nhook) {
        fn_hook = __flexiblas_hooks->slasd7.f77_hook_function[hook_pos_slasd7];
        fn_hook((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info);
    } else {
        hook_pos_slasd7 = 0;
		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info); 
	}
	return;
}

void flexiblas_chain_slasd7(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info)  __attribute__((alias("flexiblas_chain_slasd7_")));




