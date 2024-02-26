//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program. If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"


#ifndef FLEXIBLAS_CHARLEN_T
#define FLEXIBLAS_CHARLEN_T
#if __GNUC__ > 7
typedef size_t flexiblas_fortran_charlen_t;
#else
typedef int flexiblas_fortran_charlen_t;
#endif
#endif

#ifndef blasint
#ifdef FLEXIBLAS_INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif
#endif



static TLS_STORE uint8_t hook_pos_dlasd6 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlasd6,DLASD6)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, double* d, double* vf, double* vl, double* alpha, double* beta, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* poles, double* difl, double* difr, double* z, blasint* k, double* c, double* s, double* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(dlasd6,DLASD6)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, double* d, double* vf, double* vl, double* alpha, double* beta, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* poles, double* difl, double* difr, double* z, blasint* k, double* c, double* s, double* work, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* d, void* vf, void* vl, void* alpha, void* beta, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* work, void* iwork, void* info);
	void (*fn_hook) (void* icompq, void* nl, void* nr, void* sqre, void* d, void* vf, void* vl, void* alpha, void* beta, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlasd6.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlasd6.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) d, (void*) vf, (void*) vl, (void*) alpha, (void*) beta, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_dlasd6 = 0;
		fn_hook((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) d, (void*) vf, (void*) vl, (void*) alpha, (void*) beta, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlasd6_(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, double* d, double* vf, double* vl, double* alpha, double* beta, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* poles, double* difl, double* difr, double* z, blasint* k, double* c, double* s, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasd6,DLASD6)))));
#else
#ifndef __APPLE__
void dlasd6(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, double* d, double* vf, double* vl, double* alpha, double* beta, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* poles, double* difl, double* difr, double* z, blasint* k, double* c, double* s, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasd6,DLASD6)))));
#else
void dlasd6(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, double* d, double* vf, double* vl, double* alpha, double* beta, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* poles, double* difl, double* difr, double* z, blasint* k, double* c, double* s, double* work, blasint* iwork, blasint* info){ FC_GLOBAL(dlasd6,DLASD6)((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) d, (void*) vf, (void*) vl, (void*) alpha, (void*) beta, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlasd6_(void* icompq, void* nl, void* nr, void* sqre, void* d, void* vf, void* vl, void* alpha, void* beta, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* work, void* iwork, void* info)
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* d, void* vf, void* vl, void* alpha, void* beta, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* work, void* iwork, void* info);

	*(void **) & fn = current_backend->lapack.dlasd6.f77_blas_function; 

		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) d, (void*) vf, (void*) vl, (void*) alpha, (void*) beta, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlasd6(void* icompq, void* nl, void* nr, void* sqre, void* d, void* vf, void* vl, void* alpha, void* beta, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_real_dlasd6_")));
#else
void flexiblas_real_dlasd6(void* icompq, void* nl, void* nr, void* sqre, void* d, void* vf, void* vl, void* alpha, void* beta, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* work, void* iwork, void* info){flexiblas_real_dlasd6_((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) d, (void*) vf, (void*) vl, (void*) alpha, (void*) beta, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlasd6_(void* icompq, void* nl, void* nr, void* sqre, void* d, void* vf, void* vl, void* alpha, void* beta, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* work, void* iwork, void* info)
{
	void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* d, void* vf, void* vl, void* alpha, void* beta, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* work, void* iwork, void* info);
	void (*fn_hook) (void* icompq, void* nl, void* nr, void* sqre, void* d, void* vf, void* vl, void* alpha, void* beta, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* work, void* iwork, void* info);

	*(void **) &fn      = current_backend->lapack.dlasd6.f77_blas_function; 

    hook_pos_dlasd6 ++;
    if( hook_pos_dlasd6 < __flexiblas_hooks->dlasd6.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlasd6.f77_hook_function[hook_pos_dlasd6];
        fn_hook((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) d, (void*) vf, (void*) vl, (void*) alpha, (void*) beta, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_dlasd6 = 0;
		fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) d, (void*) vf, (void*) vl, (void*) alpha, (void*) beta, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlasd6(void* icompq, void* nl, void* nr, void* sqre, void* d, void* vf, void* vl, void* alpha, void* beta, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_chain_dlasd6_")));
#else
void flexiblas_chain_dlasd6(void* icompq, void* nl, void* nr, void* sqre, void* d, void* vf, void* vl, void* alpha, void* beta, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* poles, void* difl, void* difr, void* z, void* k, void* c, void* s, void* work, void* iwork, void* info){flexiblas_chain_dlasd6_((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) d, (void*) vf, (void*) vl, (void*) alpha, (void*) beta, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) poles, (void*) difl, (void*) difr, (void*) z, (void*) k, (void*) c, (void*) s, (void*) work, (void*) iwork, (void*) info);}
#endif



