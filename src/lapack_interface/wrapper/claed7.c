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



static TLS_STORE uint8_t hook_pos_claed7 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claed7,CLAED7)(blasint* n, blasint* cutpnt, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float complex* q, blasint* ldq, float* rho, blasint* indxq, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float complex* work, float* rwork, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(claed7,CLAED7)(blasint* n, blasint* cutpnt, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float complex* q, blasint* ldq, float* rho, blasint* indxq, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float complex* work, float* rwork, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* n, void* cutpnt, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* rho, void* indxq, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* rwork, void* iwork, void* info);
	void (*fn_hook) (void* n, void* cutpnt, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* rho, void* indxq, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* rwork, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.claed7.f77_blas_function; 
	fn_hook = __flexiblas_hooks->claed7.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) cutpnt, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) indxq, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) rwork, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_claed7 = 0;
		fn_hook((void*) n, (void*) cutpnt, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) indxq, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) rwork, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void claed7_(blasint* n, blasint* cutpnt, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float complex* q, blasint* ldq, float* rho, blasint* indxq, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float complex* work, float* rwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(claed7,CLAED7)))));
#else
void claed7(blasint* n, blasint* cutpnt, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float complex* q, blasint* ldq, float* rho, blasint* indxq, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float complex* work, float* rwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(claed7,CLAED7)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claed7_(void* n, void* cutpnt, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* rho, void* indxq, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* rwork, void* iwork, void* info)
{
	void (*fn) (void* n, void* cutpnt, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* rho, void* indxq, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* rwork, void* iwork, void* info);

	fn = current_backend->lapack.claed7.f77_blas_function; 

		fn((void*) n, (void*) cutpnt, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) indxq, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) rwork, (void*) iwork, (void*) info); 

	return;
}

void flexiblas_real_claed7(void* n, void* cutpnt, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* rho, void* indxq, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* rwork, void* iwork, void* info)  __attribute__((alias("flexiblas_real_claed7_")));





/* Chainloader for Hooks */


void flexiblas_chain_claed7_(void* n, void* cutpnt, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* rho, void* indxq, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* rwork, void* iwork, void* info)
{
	void (*fn) (void* n, void* cutpnt, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* rho, void* indxq, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* rwork, void* iwork, void* info);
	void (*fn_hook) (void* n, void* cutpnt, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* rho, void* indxq, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* rwork, void* iwork, void* info);

	fn      = current_backend->lapack.claed7.f77_blas_function; 

    hook_pos_claed7 ++;
    if( hook_pos_claed7 < __flexiblas_hooks->claed7.nhook) {
        fn_hook = __flexiblas_hooks->claed7.f77_hook_function[hook_pos_claed7];
        fn_hook((void*) n, (void*) cutpnt, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) indxq, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) rwork, (void*) iwork, (void*) info);
    } else {
        hook_pos_claed7 = 0;
		fn((void*) n, (void*) cutpnt, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) indxq, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) rwork, (void*) iwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_claed7(void* n, void* cutpnt, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* rho, void* indxq, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* rwork, void* iwork, void* info)  __attribute__((alias("flexiblas_chain_claed7_")));




