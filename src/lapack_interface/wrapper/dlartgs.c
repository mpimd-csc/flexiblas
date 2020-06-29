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



static TLS_STORE uint8_t hook_pos_dlartgs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlartgs,DLARTGS)(double* x, double* y, double* sigma, double* cs, double* sn)
#else
void FC_GLOBAL(dlartgs,DLARTGS)(double* x, double* y, double* sigma, double* cs, double* sn)
#endif
{
	void (*fn) (void* x, void* y, void* sigma, void* cs, void* sn);
	void (*fn_hook) (void* x, void* y, void* sigma, void* cs, void* sn);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlartgs.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlartgs.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn); 
		return;
	} else {
		hook_pos_dlartgs = 0;
		fn_hook((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlartgs_(double* x, double* y, double* sigma, double* cs, double* sn) __attribute__((alias(MTS(FC_GLOBAL(dlartgs,DLARTGS)))));
#else
void dlartgs(double* x, double* y, double* sigma, double* cs, double* sn) __attribute__((alias(MTS(FC_GLOBAL(dlartgs,DLARTGS)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlartgs_(void* x, void* y, void* sigma, void* cs, void* sn)
{
	void (*fn) (void* x, void* y, void* sigma, void* cs, void* sn);

	fn = current_backend->lapack.dlartgs.f77_blas_function; 

		fn((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn); 

	return;
}

void flexiblas_real_dlartgs(void* x, void* y, void* sigma, void* cs, void* sn)  __attribute__((alias("flexiblas_real_dlartgs_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlartgs_(void* x, void* y, void* sigma, void* cs, void* sn)
{
	void (*fn) (void* x, void* y, void* sigma, void* cs, void* sn);
	void (*fn_hook) (void* x, void* y, void* sigma, void* cs, void* sn);

	fn      = current_backend->lapack.dlartgs.f77_blas_function; 

    hook_pos_dlartgs ++;
    if( hook_pos_dlartgs < __flexiblas_hooks->dlartgs.nhook) {
        fn_hook = __flexiblas_hooks->dlartgs.f77_hook_function[hook_pos_dlartgs];
        fn_hook((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn);
    } else {
        hook_pos_dlartgs = 0;
		fn((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn); 
	}
	return;
}

void flexiblas_chain_dlartgs(void* x, void* y, void* sigma, void* cs, void* sn)  __attribute__((alias("flexiblas_chain_dlartgs_")));




