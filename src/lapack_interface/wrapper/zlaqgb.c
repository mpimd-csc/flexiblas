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



static TLS_STORE uint8_t hook_pos_zlaqgb = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlaqgb,ZLAQGB)(blasint* m, blasint* n, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed)
#else
void FC_GLOBAL(zlaqgb,ZLAQGB)(blasint* m, blasint* n, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed)
#endif
{
	void (*fn) (void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed);
	void (*fn_hook) (void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlaqgb.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlaqgb.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed); 
		return;
	} else {
		hook_pos_zlaqgb = 0;
		fn_hook((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlaqgb_(blasint* m, blasint* n, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed) __attribute__((alias(MTS(FC_GLOBAL(zlaqgb,ZLAQGB)))));
#else
void zlaqgb(blasint* m, blasint* n, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed) __attribute__((alias(MTS(FC_GLOBAL(zlaqgb,ZLAQGB)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaqgb_(void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed)
{
	void (*fn) (void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed);

	fn = current_backend->lapack.zlaqgb.f77_blas_function; 

		fn((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed); 

	return;
}

void flexiblas_real_zlaqgb(void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed)  __attribute__((alias("flexiblas_real_zlaqgb_")));





/* Chainloader for Hooks */


void flexiblas_chain_zlaqgb_(void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed)
{
	void (*fn) (void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed);
	void (*fn_hook) (void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed);

	fn      = current_backend->lapack.zlaqgb.f77_blas_function; 

    hook_pos_zlaqgb ++;
    if( hook_pos_zlaqgb < __flexiblas_hooks->zlaqgb.nhook) {
        fn_hook = __flexiblas_hooks->zlaqgb.f77_hook_function[hook_pos_zlaqgb];
        fn_hook((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed);
    } else {
        hook_pos_zlaqgb = 0;
		fn((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed); 
	}
	return;
}

void flexiblas_chain_zlaqgb(void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed)  __attribute__((alias("flexiblas_chain_zlaqgb_")));




