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
 /* Generated: Wed Mar 28 11:20:05 2018 */
        
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



static TLS_STORE uint8_t hook_pos_zlals0 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlals0,ZLALS0)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* nrhs, double complex* b, blasint* ldb, double complex* bx, blasint* ldbx, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* poles, double* difl, double* difr, double* z, blasint* k, double* c, double* s, double* rwork, blasint* info)
#else
void FC_GLOBAL(zlals0,ZLALS0)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* nrhs, double complex* b, blasint* ldb, double complex* bx, blasint* ldbx, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* poles, double* difl, double* difr, double* z, blasint* k, double* c, double* s, double* rwork, blasint* info)
#endif
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info);
	void (*fn_hook) (void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlals0.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlals0.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) rwork, (void*) info); 
		return;
	} else {
		hook_pos_zlals0 = 0;
		fn_hook((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) rwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlals0_(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* nrhs, double complex* b, blasint* ldb, double complex* bx, blasint* ldbx, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* poles, double* difl, double* difr, double* z, blasint* k, double* c, double* s, double* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zlals0,ZLALS0)))));
#else
void zlals0(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* nrhs, double complex* b, blasint* ldb, double complex* bx, blasint* ldbx, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* poles, double* difl, double* difr, double* z, blasint* k, double* c, double* s, double* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zlals0,ZLALS0)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlals0_(void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info)
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info);

	fn = current_backend->lapack.zlals0.f77_blas_function; 

		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) rwork, (void*) info); 

	return;
}

void flexiblas_real_zlals0(void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info)  __attribute__((alias("flexiblas_real_zlals0_")));





/* Chainloader for Hooks */


void flexiblas_chain_zlals0_(void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info)
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info);
	void (*fn_hook) (void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info);

	fn      = current_backend->lapack.zlals0.f77_blas_function; 

    hook_pos_zlals0 ++;
    if( hook_pos_zlals0 < __flexiblas_hooks->zlals0.nhook) {
        fn_hook = __flexiblas_hooks->zlals0.f77_hook_function[hook_pos_zlals0];
        fn_hook((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) rwork, (void*) info);
    } else {
        hook_pos_zlals0 = 0;
		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) rwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_zlals0(void* icompq, void* nl, void* nr, void* sqre, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* rwork, void* info)  __attribute__((alias("flexiblas_chain_zlals0_")));




