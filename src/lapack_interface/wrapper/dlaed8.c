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


#if __GNUC__ > 7
typedef size_t fortran_charlen_t;
#else
typedef int fortran_charlen_t;
#endif

#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_dlaed8 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlaed8,DLAED8)(blasint* icompq, blasint* k, blasint* n, blasint* qsiz, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, blasint* cutpnt, double* z, double* dlambda, double* q2, blasint* ldq2, double* w, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* indxp, blasint* indx, blasint* info)
#else
void FC_GLOBAL(dlaed8,DLAED8)(blasint* icompq, blasint* k, blasint* n, blasint* qsiz, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, blasint* cutpnt, double* z, double* dlambda, double* q2, blasint* ldq2, double* w, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* indxp, blasint* indx, blasint* info)
#endif
{
	void (*fn) (void* icompq, void* k, void* n, void* qsiz, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* perm, void* givptr, void* givcol, void* givnum, void* indxp, void* indx, void* info);
	void (*fn_hook) (void* icompq, void* k, void* n, void* qsiz, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* perm, void* givptr, void* givcol, void* givnum, void* indxp, void* indx, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlaed8.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlaed8.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) icompq, (void*) k, (void*) n, (void*) qsiz, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) indxp, (void*) indx, (void*) info); 
		return;
	} else {
		hook_pos_dlaed8 = 0;
		fn_hook((void*) icompq, (void*) k, (void*) n, (void*) qsiz, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) indxp, (void*) indx, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlaed8_(blasint* icompq, blasint* k, blasint* n, blasint* qsiz, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, blasint* cutpnt, double* z, double* dlambda, double* q2, blasint* ldq2, double* w, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* indxp, blasint* indx, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlaed8,DLAED8)))));
#else
#ifndef __APPLE__
void dlaed8(blasint* icompq, blasint* k, blasint* n, blasint* qsiz, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, blasint* cutpnt, double* z, double* dlambda, double* q2, blasint* ldq2, double* w, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* indxp, blasint* indx, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlaed8,DLAED8)))));
#else
void dlaed8(blasint* icompq, blasint* k, blasint* n, blasint* qsiz, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, blasint* cutpnt, double* z, double* dlambda, double* q2, blasint* ldq2, double* w, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* indxp, blasint* indx, blasint* info){ FC_GLOBAL(dlaed8,DLAED8)((void*) icompq, (void*) k, (void*) n, (void*) qsiz, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) indxp, (void*) indx, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlaed8_(void* icompq, void* k, void* n, void* qsiz, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* perm, void* givptr, void* givcol, void* givnum, void* indxp, void* indx, void* info)
{
	void (*fn) (void* icompq, void* k, void* n, void* qsiz, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* perm, void* givptr, void* givcol, void* givnum, void* indxp, void* indx, void* info);

	*(void **) & fn = current_backend->lapack.dlaed8.f77_blas_function; 

		fn((void*) icompq, (void*) k, (void*) n, (void*) qsiz, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) indxp, (void*) indx, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlaed8(void* icompq, void* k, void* n, void* qsiz, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* perm, void* givptr, void* givcol, void* givnum, void* indxp, void* indx, void* info) __attribute__((alias("flexiblas_real_dlaed8_")));
#else
void flexiblas_real_dlaed8(void* icompq, void* k, void* n, void* qsiz, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* perm, void* givptr, void* givcol, void* givnum, void* indxp, void* indx, void* info){flexiblas_real_dlaed8_((void*) icompq, (void*) k, (void*) n, (void*) qsiz, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) indxp, (void*) indx, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlaed8_(void* icompq, void* k, void* n, void* qsiz, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* perm, void* givptr, void* givcol, void* givnum, void* indxp, void* indx, void* info)
{
	void (*fn) (void* icompq, void* k, void* n, void* qsiz, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* perm, void* givptr, void* givcol, void* givnum, void* indxp, void* indx, void* info);
	void (*fn_hook) (void* icompq, void* k, void* n, void* qsiz, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* perm, void* givptr, void* givcol, void* givnum, void* indxp, void* indx, void* info);

	*(void **) &fn      = current_backend->lapack.dlaed8.f77_blas_function; 

    hook_pos_dlaed8 ++;
    if( hook_pos_dlaed8 < __flexiblas_hooks->dlaed8.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlaed8.f77_hook_function[hook_pos_dlaed8];
        fn_hook((void*) icompq, (void*) k, (void*) n, (void*) qsiz, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) indxp, (void*) indx, (void*) info);
    } else {
        hook_pos_dlaed8 = 0;
		fn((void*) icompq, (void*) k, (void*) n, (void*) qsiz, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) indxp, (void*) indx, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlaed8(void* icompq, void* k, void* n, void* qsiz, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* perm, void* givptr, void* givcol, void* givnum, void* indxp, void* indx, void* info) __attribute__((alias("flexiblas_chain_dlaed8_")));
#else
void flexiblas_chain_dlaed8(void* icompq, void* k, void* n, void* qsiz, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* perm, void* givptr, void* givcol, void* givnum, void* indxp, void* indx, void* info){flexiblas_chain_dlaed8_((void*) icompq, (void*) k, (void*) n, (void*) qsiz, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) indxp, (void*) indx, (void*) info);}
#endif



