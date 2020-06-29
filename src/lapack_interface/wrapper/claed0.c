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



static TLS_STORE uint8_t hook_pos_claed0 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claed0,CLAED0)(blasint* qsiz, blasint* n, float* d, float* e, float complex* q, blasint* ldq, float complex* qstore, blasint* ldqs, float* rwork, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(claed0,CLAED0)(blasint* qsiz, blasint* n, float* d, float* e, float complex* q, blasint* ldq, float complex* qstore, blasint* ldqs, float* rwork, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* qsiz, void* n, void* d, void* e, void* q, void* ldq, void* qstore, void* ldqs, void* rwork, void* iwork, void* info);
	void (*fn_hook) (void* qsiz, void* n, void* d, void* e, void* q, void* ldq, void* qstore, void* ldqs, void* rwork, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.claed0.f77_blas_function; 
	fn_hook = __flexiblas_hooks->claed0.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) qsiz, (void*) n, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) qstore, (void*) ldqs, (void*) rwork, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_claed0 = 0;
		fn_hook((void*) qsiz, (void*) n, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) qstore, (void*) ldqs, (void*) rwork, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void claed0_(blasint* qsiz, blasint* n, float* d, float* e, float complex* q, blasint* ldq, float complex* qstore, blasint* ldqs, float* rwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(claed0,CLAED0)))));
#else
void claed0(blasint* qsiz, blasint* n, float* d, float* e, float complex* q, blasint* ldq, float complex* qstore, blasint* ldqs, float* rwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(claed0,CLAED0)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claed0_(void* qsiz, void* n, void* d, void* e, void* q, void* ldq, void* qstore, void* ldqs, void* rwork, void* iwork, void* info)
{
	void (*fn) (void* qsiz, void* n, void* d, void* e, void* q, void* ldq, void* qstore, void* ldqs, void* rwork, void* iwork, void* info);

	fn = current_backend->lapack.claed0.f77_blas_function; 

		fn((void*) qsiz, (void*) n, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) qstore, (void*) ldqs, (void*) rwork, (void*) iwork, (void*) info); 

	return;
}

void flexiblas_real_claed0(void* qsiz, void* n, void* d, void* e, void* q, void* ldq, void* qstore, void* ldqs, void* rwork, void* iwork, void* info)  __attribute__((alias("flexiblas_real_claed0_")));





/* Chainloader for Hooks */


void flexiblas_chain_claed0_(void* qsiz, void* n, void* d, void* e, void* q, void* ldq, void* qstore, void* ldqs, void* rwork, void* iwork, void* info)
{
	void (*fn) (void* qsiz, void* n, void* d, void* e, void* q, void* ldq, void* qstore, void* ldqs, void* rwork, void* iwork, void* info);
	void (*fn_hook) (void* qsiz, void* n, void* d, void* e, void* q, void* ldq, void* qstore, void* ldqs, void* rwork, void* iwork, void* info);

	fn      = current_backend->lapack.claed0.f77_blas_function; 

    hook_pos_claed0 ++;
    if( hook_pos_claed0 < __flexiblas_hooks->claed0.nhook) {
        fn_hook = __flexiblas_hooks->claed0.f77_hook_function[hook_pos_claed0];
        fn_hook((void*) qsiz, (void*) n, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) qstore, (void*) ldqs, (void*) rwork, (void*) iwork, (void*) info);
    } else {
        hook_pos_claed0 = 0;
		fn((void*) qsiz, (void*) n, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) qstore, (void*) ldqs, (void*) rwork, (void*) iwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_claed0(void* qsiz, void* n, void* d, void* e, void* q, void* ldq, void* qstore, void* ldqs, void* rwork, void* iwork, void* info)  __attribute__((alias("flexiblas_chain_claed0_")));




