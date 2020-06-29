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



static TLS_STORE uint8_t hook_pos_claein = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claein,CLAEIN)(blasint* rightv, blasint* noinit, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* v, float complex* b, blasint* ldb, float* rwork, float* eps3, float* smlnum, blasint* info)
#else
void FC_GLOBAL(claein,CLAEIN)(blasint* rightv, blasint* noinit, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* v, float complex* b, blasint* ldb, float* rwork, float* eps3, float* smlnum, blasint* info)
#endif
{
	void (*fn) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info);
	void (*fn_hook) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.claein.f77_blas_function; 
	fn_hook = __flexiblas_hooks->claein.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info); 
		return;
	} else {
		hook_pos_claein = 0;
		fn_hook((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void claein_(blasint* rightv, blasint* noinit, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* v, float complex* b, blasint* ldb, float* rwork, float* eps3, float* smlnum, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(claein,CLAEIN)))));
#else
void claein(blasint* rightv, blasint* noinit, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* v, float complex* b, blasint* ldb, float* rwork, float* eps3, float* smlnum, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(claein,CLAEIN)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claein_(void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info)
{
	void (*fn) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info);

	fn = current_backend->lapack.claein.f77_blas_function; 

		fn((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info); 

	return;
}

void flexiblas_real_claein(void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info)  __attribute__((alias("flexiblas_real_claein_")));





/* Chainloader for Hooks */


void flexiblas_chain_claein_(void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info)
{
	void (*fn) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info);
	void (*fn_hook) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info);

	fn      = current_backend->lapack.claein.f77_blas_function; 

    hook_pos_claein ++;
    if( hook_pos_claein < __flexiblas_hooks->claein.nhook) {
        fn_hook = __flexiblas_hooks->claein.f77_hook_function[hook_pos_claein];
        fn_hook((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info);
    } else {
        hook_pos_claein = 0;
		fn((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info); 
	}
	return;
}

void flexiblas_chain_claein(void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info)  __attribute__((alias("flexiblas_chain_claein_")));




