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



static TLS_STORE uint8_t hook_pos_slamrg = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slamrg,SLAMRG)(blasint* n1, blasint* n2, float* a, blasint* strd1, blasint* strd2, blasint* index_bn)
#else
void FC_GLOBAL(slamrg,SLAMRG)(blasint* n1, blasint* n2, float* a, blasint* strd1, blasint* strd2, blasint* index_bn)
#endif
{
	void (*fn) (void* n1, void* n2, void* a, void* strd1, void* strd2, void* index_bn);
	void (*fn_hook) (void* n1, void* n2, void* a, void* strd1, void* strd2, void* index_bn);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slamrg.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slamrg.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n1, (void*) n2, (void*) a, (void*) strd1, (void*) strd2, (void*) index_bn); 
		return;
	} else {
		hook_pos_slamrg = 0;
		fn_hook((void*) n1, (void*) n2, (void*) a, (void*) strd1, (void*) strd2, (void*) index_bn);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slamrg_(blasint* n1, blasint* n2, float* a, blasint* strd1, blasint* strd2, blasint* index_bn) __attribute__((alias(MTS(FC_GLOBAL(slamrg,SLAMRG)))));
#else
void slamrg(blasint* n1, blasint* n2, float* a, blasint* strd1, blasint* strd2, blasint* index_bn) __attribute__((alias(MTS(FC_GLOBAL(slamrg,SLAMRG)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slamrg_(void* n1, void* n2, void* a, void* strd1, void* strd2, void* index_bn)
{
	void (*fn) (void* n1, void* n2, void* a, void* strd1, void* strd2, void* index_bn);

	fn = current_backend->lapack.slamrg.f77_blas_function; 

		fn((void*) n1, (void*) n2, (void*) a, (void*) strd1, (void*) strd2, (void*) index_bn); 

	return;
}

void flexiblas_real_slamrg(void* n1, void* n2, void* a, void* strd1, void* strd2, void* index_bn)  __attribute__((alias("flexiblas_real_slamrg_")));





/* Chainloader for Hooks */


void flexiblas_chain_slamrg_(void* n1, void* n2, void* a, void* strd1, void* strd2, void* index_bn)
{
	void (*fn) (void* n1, void* n2, void* a, void* strd1, void* strd2, void* index_bn);
	void (*fn_hook) (void* n1, void* n2, void* a, void* strd1, void* strd2, void* index_bn);

	fn      = current_backend->lapack.slamrg.f77_blas_function; 

    hook_pos_slamrg ++;
    if( hook_pos_slamrg < __flexiblas_hooks->slamrg.nhook) {
        fn_hook = __flexiblas_hooks->slamrg.f77_hook_function[hook_pos_slamrg];
        fn_hook((void*) n1, (void*) n2, (void*) a, (void*) strd1, (void*) strd2, (void*) index_bn);
    } else {
        hook_pos_slamrg = 0;
		fn((void*) n1, (void*) n2, (void*) a, (void*) strd1, (void*) strd2, (void*) index_bn); 
	}
	return;
}

void flexiblas_chain_slamrg(void* n1, void* n2, void* a, void* strd1, void* strd2, void* index_bn)  __attribute__((alias("flexiblas_chain_slamrg_")));




