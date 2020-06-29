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



static TLS_STORE uint8_t hook_pos_dlasd7 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlasd7,DLASD7)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* z, double* zw, double* vf, double* vfw, double* vl, double* vlw, double* alpha, double* beta, double* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* c, double* s, blasint* info)
#else
void FC_GLOBAL(dlasd7,DLASD7)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* z, double* zw, double* vf, double* vfw, double* vl, double* vlw, double* alpha, double* beta, double* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* c, double* s, blasint* info)
#endif
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);
	void (*fn_hook) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlasd7.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlasd7.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info); 
		return;
	} else {
		hook_pos_dlasd7 = 0;
		fn_hook((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlasd7_(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* z, double* zw, double* vf, double* vfw, double* vl, double* vlw, double* alpha, double* beta, double* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* c, double* s, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasd7,DLASD7)))));
#else
void dlasd7(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* z, double* zw, double* vf, double* vfw, double* vl, double* vlw, double* alpha, double* beta, double* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* c, double* s, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasd7,DLASD7)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlasd7_(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info)
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);

	fn = current_backend->lapack.dlasd7.f77_blas_function; 

		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info); 

	return;
}

void flexiblas_real_dlasd7(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info)  __attribute__((alias("flexiblas_real_dlasd7_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlasd7_(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info)
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);
	void (*fn_hook) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);

	fn      = current_backend->lapack.dlasd7.f77_blas_function; 

    hook_pos_dlasd7 ++;
    if( hook_pos_dlasd7 < __flexiblas_hooks->dlasd7.nhook) {
        fn_hook = __flexiblas_hooks->dlasd7.f77_hook_function[hook_pos_dlasd7];
        fn_hook((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info);
    } else {
        hook_pos_dlasd7 = 0;
		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info); 
	}
	return;
}

void flexiblas_chain_dlasd7(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info)  __attribute__((alias("flexiblas_chain_dlasd7_")));




