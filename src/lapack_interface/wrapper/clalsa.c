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

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_clalsa = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clalsa,CLALSA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* nrhs, float complex* b, blasint* ldb, float complex* bx, blasint* ldbx, float* u, blasint* ldu, float* vt, blasint* k, float* difl, float* difr, float* z, float* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, float* givnum, float* c, float* s, float* rwork, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(clalsa,CLALSA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* nrhs, float complex* b, blasint* ldb, float complex* bx, blasint* ldbx, float* u, blasint* ldu, float* vt, blasint* k, float* difl, float* difr, float* z, float* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, float* givnum, float* c, float* s, float* rwork, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* icompq, void* smlsiz, void* n, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* rwork, void* iwork, void* info);
	void (*fn_hook) (void* icompq, void* smlsiz, void* n, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* rwork, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.clalsa.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->clalsa.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) icompq, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) rwork, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_clalsa = 0;
		fn_hook((void*) icompq, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) rwork, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clalsa_(blasint* icompq, blasint* smlsiz, blasint* n, blasint* nrhs, float complex* b, blasint* ldb, float complex* bx, blasint* ldbx, float* u, blasint* ldu, float* vt, blasint* k, float* difl, float* difr, float* z, float* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, float* givnum, float* c, float* s, float* rwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(clalsa,CLALSA)))));
#else
#ifndef __APPLE__
void clalsa(blasint* icompq, blasint* smlsiz, blasint* n, blasint* nrhs, float complex* b, blasint* ldb, float complex* bx, blasint* ldbx, float* u, blasint* ldu, float* vt, blasint* k, float* difl, float* difr, float* z, float* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, float* givnum, float* c, float* s, float* rwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(clalsa,CLALSA)))));
#else
void clalsa(blasint* icompq, blasint* smlsiz, blasint* n, blasint* nrhs, float complex* b, blasint* ldb, float complex* bx, blasint* ldbx, float* u, blasint* ldu, float* vt, blasint* k, float* difl, float* difr, float* z, float* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, float* givnum, float* c, float* s, float* rwork, blasint* iwork, blasint* info){ FC_GLOBAL(clalsa,CLALSA)((void*) icompq, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) rwork, (void*) iwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clalsa_(void* icompq, void* smlsiz, void* n, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* rwork, void* iwork, void* info)
{
	void (*fn) (void* icompq, void* smlsiz, void* n, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* rwork, void* iwork, void* info);

	*(void **) & fn = current_backend->lapack.clalsa.f77_blas_function; 

		fn((void*) icompq, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) rwork, (void*) iwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_clalsa(void* icompq, void* smlsiz, void* n, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* rwork, void* iwork, void* info) __attribute__((alias("flexiblas_real_clalsa_")));
#else
void flexiblas_real_clalsa(void* icompq, void* smlsiz, void* n, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* rwork, void* iwork, void* info){flexiblas_real_clalsa_((void*) icompq, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) rwork, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clalsa_(void* icompq, void* smlsiz, void* n, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* rwork, void* iwork, void* info)
{
	void (*fn) (void* icompq, void* smlsiz, void* n, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* rwork, void* iwork, void* info);
	void (*fn_hook) (void* icompq, void* smlsiz, void* n, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* rwork, void* iwork, void* info);

	*(void **) &fn      = current_backend->lapack.clalsa.f77_blas_function; 

    hook_pos_clalsa ++;
    if( hook_pos_clalsa < __flexiblas_hooks->clalsa.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clalsa.f77_hook_function[hook_pos_clalsa];
        fn_hook((void*) icompq, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) rwork, (void*) iwork, (void*) info);
    } else {
        hook_pos_clalsa = 0;
		fn((void*) icompq, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) rwork, (void*) iwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_clalsa(void* icompq, void* smlsiz, void* n, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* rwork, void* iwork, void* info) __attribute__((alias("flexiblas_chain_clalsa_")));
#else
void flexiblas_chain_clalsa(void* icompq, void* smlsiz, void* n, void* nrhs, void* b, void* ldb, void* bx, void* ldbx, void* u, void* ldu, void* vt, void* k, void* difl, void* difr, void* z, void* poles, void* givptr, void* givcol, void* ldgcol, void* perm, void* givnum, void* c, void* s, void* rwork, void* iwork, void* info){flexiblas_chain_clalsa_((void*) icompq, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) b, (void*) ldb, (void*) bx, (void*) ldbx, (void*) u, (void*) ldu, (void*) vt, (void*) k, (void*) difl, (void*) difr, (void*) z, (void*) poles, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) perm, (void*) givnum, (void*) c, (void*) s, (void*) rwork, (void*) iwork, (void*) info);}
#endif



