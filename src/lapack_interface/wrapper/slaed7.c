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



static TLS_STORE uint8_t hook_pos_slaed7 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaed7,SLAED7)(blasint* icompq, blasint* n, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(slaed7,SLAED7)(blasint* icompq, blasint* n, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float* work, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info);
	void (*fn_hook) (void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slaed7.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slaed7.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_slaed7 = 0;
		fn_hook((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slaed7_(blasint* icompq, blasint* n, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaed7,SLAED7)))));
#else
void slaed7(blasint* icompq, blasint* n, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaed7,SLAED7)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaed7_(void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info)
{
	void (*fn) (void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info);

	fn = current_backend->lapack.slaed7.f77_blas_function; 

		fn((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info); 

	return;
}

void flexiblas_real_slaed7(void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info)  __attribute__((alias("flexiblas_real_slaed7_")));





/* Chainloader for Hooks */


void flexiblas_chain_slaed7_(void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info)
{
	void (*fn) (void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info);
	void (*fn_hook) (void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info);

	fn      = current_backend->lapack.slaed7.f77_blas_function; 

    hook_pos_slaed7 ++;
    if( hook_pos_slaed7 < __flexiblas_hooks->slaed7.nhook) {
        fn_hook = __flexiblas_hooks->slaed7.f77_hook_function[hook_pos_slaed7];
        fn_hook((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_slaed7 = 0;
		fn((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_slaed7(void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info)  __attribute__((alias("flexiblas_chain_slaed7_")));




