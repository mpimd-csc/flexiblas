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



static TLS_STORE uint8_t hook_pos_clals0 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clals0,CLALS0)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* nrhs, float complex* b, blasint* ldb, float complex* bx, blasint* ldbx, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* poles, float* difl, float* difr, float* z, blasint* k, float* c, float* s, float* rwork, blasint* info)
#else
void FC_GLOBAL(clals0,CLALS0)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* nrhs, float complex* b, blasint* ldb, float complex* bx, blasint* ldbx, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* poles, float* difl, float* difr, float* z, blasint* k, float* c, float* s, float* rwork, blasint* info)
#endif
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info);
	void (*fn_hook) (void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.clals0.f77_blas_function; 
	fn_hook = __flexiblas_hooks->clals0.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) rwork, (void*) info); 
		return;
	} else {
		hook_pos_clals0 = 0;
		fn_hook((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) rwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clals0_(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* nrhs, float complex* b, blasint* ldb, float complex* bx, blasint* ldbx, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* poles, float* difl, float* difr, float* z, blasint* k, float* c, float* s, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(clals0,CLALS0)))));
#else
void clals0(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* nrhs, float complex* b, blasint* ldb, float complex* bx, blasint* ldbx, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* poles, float* difl, float* difr, float* z, blasint* k, float* c, float* s, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(clals0,CLALS0)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clals0_(void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info)
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info);

	fn = current_backend->lapack.clals0.f77_blas_function; 

		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) rwork, (void*) info); 

	return;
}

void flexiblas_real_clals0(void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info)  __attribute__((alias("flexiblas_real_clals0_")));





/* Chainloader for Hooks */


void flexiblas_chain_clals0_(void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info)
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info);
	void (*fn_hook) (void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info);

	fn      = current_backend->lapack.clals0.f77_blas_function; 

    hook_pos_clals0 ++;
    if( hook_pos_clals0 < __flexiblas_hooks->clals0.nhook) {
        fn_hook = __flexiblas_hooks->clals0.f77_hook_function[hook_pos_clals0];
        fn_hook((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) rwork, (void*) info);
    } else {
        hook_pos_clals0 = 0;
		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) rwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_clals0(void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info)  __attribute__((alias("flexiblas_chain_clals0_")));




