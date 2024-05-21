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


static TLS_STORE uint8_t hook_pos_claed8 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claed8,CLAED8)(blasint* k, blasint* n, blasint* qsiz, float complex* q, blasint* ldq, float* d, float* rho, blasint* cutpnt, float* z, float* dlambda, float complex* q2, blasint* ldq2, float* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, blasint* info)
#else
void FC_GLOBAL(claed8,CLAED8)(blasint* k, blasint* n, blasint* qsiz, float complex* q, blasint* ldq, float* d, float* rho, blasint* cutpnt, float* z, float* dlambda, float complex* q2, blasint* ldq2, float* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, blasint* info)
#endif
{
	void (*fn) (void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info);
	void (*fn_hook) (void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.claed8.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->claed8.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info); 
		return;
	} else {
		hook_pos_claed8 = 0;
		fn_hook((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void claed8_(blasint* k, blasint* n, blasint* qsiz, float complex* q, blasint* ldq, float* d, float* rho, blasint* cutpnt, float* z, float* dlambda, float complex* q2, blasint* ldq2, float* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(claed8,CLAED8)))));
#else
#ifndef __APPLE__
void claed8(blasint* k, blasint* n, blasint* qsiz, float complex* q, blasint* ldq, float* d, float* rho, blasint* cutpnt, float* z, float* dlambda, float complex* q2, blasint* ldq2, float* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(claed8,CLAED8)))));
#else
void claed8(blasint* k, blasint* n, blasint* qsiz, float complex* q, blasint* ldq, float* d, float* rho, blasint* cutpnt, float* z, float* dlambda, float complex* q2, blasint* ldq2, float* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, blasint* info){ FC_GLOBAL(claed8,CLAED8)((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claed8_(void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info)
{
	void (*fn) (void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info);

	*(void **) & fn = current_backend->lapack.claed8.f77_blas_function; 

		fn((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_claed8(void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info) __attribute__((alias("flexiblas_real_claed8_")));
#else
void flexiblas_real_claed8(void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info){flexiblas_real_claed8_((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_claed8_(void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info)
{
	void (*fn) (void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info);
	void (*fn_hook) (void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info);

	*(void **) &fn      = current_backend->lapack.claed8.f77_blas_function; 

    hook_pos_claed8 ++;
    if( hook_pos_claed8 < __flexiblas_hooks->claed8.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->claed8.f77_hook_function[hook_pos_claed8];
        fn_hook((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info);
    } else {
        hook_pos_claed8 = 0;
		fn((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_claed8(void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info) __attribute__((alias("flexiblas_chain_claed8_")));
#else
void flexiblas_chain_claed8(void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info){flexiblas_chain_claed8_((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info);}
#endif



