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



static TLS_STORE uint8_t hook_pos_dlasd3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlasd3,DLASD3)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* q, blasint* ldq, double* dsigma, double* u, blasint* ldu, double* u2, blasint* ldu2, double* vt, blasint* ldvt, double* vt2, blasint* ldvt2, blasint* idxc, blasint* ctot, double* z, blasint* info)
#else
void FC_GLOBAL(dlasd3,DLASD3)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* q, blasint* ldq, double* dsigma, double* u, blasint* ldu, double* u2, blasint* ldu2, double* vt, blasint* ldvt, double* vt2, blasint* ldvt2, blasint* idxc, blasint* ctot, double* z, blasint* info)
#endif
{
	void (*fn) (void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info);
	void (*fn_hook) (void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlasd3.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlasd3.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info); 
		return;
	} else {
		hook_pos_dlasd3 = 0;
		fn_hook((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlasd3_(blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* q, blasint* ldq, double* dsigma, double* u, blasint* ldu, double* u2, blasint* ldu2, double* vt, blasint* ldvt, double* vt2, blasint* ldvt2, blasint* idxc, blasint* ctot, double* z, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasd3,DLASD3)))));
#else
void dlasd3(blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* q, blasint* ldq, double* dsigma, double* u, blasint* ldu, double* u2, blasint* ldu2, double* vt, blasint* ldvt, double* vt2, blasint* ldvt2, blasint* idxc, blasint* ctot, double* z, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasd3,DLASD3)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlasd3_(void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info)
{
	void (*fn) (void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info);

	fn = current_backend->lapack.dlasd3.f77_blas_function; 

		fn((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info); 

	return;
}

void flexiblas_real_dlasd3(void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info)  __attribute__((alias("flexiblas_real_dlasd3_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlasd3_(void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info)
{
	void (*fn) (void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info);
	void (*fn_hook) (void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info);

	fn      = current_backend->lapack.dlasd3.f77_blas_function; 

    hook_pos_dlasd3 ++;
    if( hook_pos_dlasd3 < __flexiblas_hooks->dlasd3.nhook) {
        fn_hook = __flexiblas_hooks->dlasd3.f77_hook_function[hook_pos_dlasd3];
        fn_hook((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info);
    } else {
        hook_pos_dlasd3 = 0;
		fn((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info); 
	}
	return;
}

void flexiblas_chain_dlasd3(void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info)  __attribute__((alias("flexiblas_chain_dlasd3_")));




