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
 /* Generated: Mon Jun  8 14:12:34 2020 */
        
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



static TLS_STORE uint8_t hook_pos_lsamen = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(lsamen,LSAMEN)(blasint* n, char* ca, char* cb, blasint len_ca, blasint len_cb)
#else
int FC_GLOBAL(lsamen,LSAMEN)(blasint* n, char* ca, char* cb, blasint len_ca, blasint len_cb)
#endif
{
	blasint (*fn) (void* n, void* ca, void* cb, blasint len_ca, blasint len_cb);
	blasint (*fn_hook) (void* n, void* ca, void* cb, blasint len_ca, blasint len_cb);
	blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.lsamen.f77_blas_function; 
	fn_hook = __flexiblas_hooks->lsamen.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) n, (void*) ca, (void*) cb, (blasint) len_ca, (blasint) len_cb); 
		return ret; 
	} else {
		hook_pos_lsamen = 0;
		ret=fn_hook((void*) n, (void*) ca, (void*) cb, (blasint) len_ca, (blasint) len_cb);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
int lsamen_(blasint* n, char* ca, char* cb, blasint len_ca, blasint len_cb) __attribute__((alias(MTS(FC_GLOBAL(lsamen,LSAMEN)))));
#else
int lsamen(blasint* n, char* ca, char* cb, blasint len_ca, blasint len_cb) __attribute__((alias(MTS(FC_GLOBAL(lsamen,LSAMEN)))));
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_lsamen_(void* n, void* ca, void* cb, blasint len_ca, blasint len_cb)
{
	blasint (*fn) (void* n, void* ca, void* cb, blasint len_ca, blasint len_cb);
	blasint ret;

	fn = current_backend->lapack.lsamen.f77_blas_function; 

		ret = fn((void*) n, (void*) ca, (void*) cb, (blasint) len_ca, (blasint) len_cb); 

	return ret ;
}

blasint flexiblas_real_lsamen(void* n, void* ca, void* cb, blasint len_ca, blasint len_cb)  __attribute__((alias("flexiblas_real_lsamen_")));





/* Chainloader for Hooks */


blasint flexiblas_chain_lsamen_(void* n, void* ca, void* cb, blasint len_ca, blasint len_cb)
{
	blasint (*fn) (void* n, void* ca, void* cb, blasint len_ca, blasint len_cb);
	blasint (*fn_hook) (void* n, void* ca, void* cb, blasint len_ca, blasint len_cb);
	blasint ret;

	fn      = current_backend->lapack.lsamen.f77_blas_function; 

    hook_pos_lsamen ++;
    if( hook_pos_lsamen < __flexiblas_hooks->lsamen.nhook) {
        fn_hook = __flexiblas_hooks->lsamen.f77_hook_function[hook_pos_lsamen];
        ret = fn_hook((void*) n, (void*) ca, (void*) cb, (blasint) len_ca, (blasint) len_cb);
    } else {
        hook_pos_lsamen = 0;
		ret = fn((void*) n, (void*) ca, (void*) cb, (blasint) len_ca, (blasint) len_cb); 
	}
	return ret ;
}

blasint flexiblas_chain_lsamen(void* n, void* ca, void* cb, blasint len_ca, blasint len_cb)  __attribute__((alias("flexiblas_chain_lsamen_")));




