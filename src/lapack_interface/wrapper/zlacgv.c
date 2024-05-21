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

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_zlacgv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlacgv,ZLACGV)(blasint* n, double complex* x, blasint* incx)
#else
void FC_GLOBAL(zlacgv,ZLACGV)(blasint* n, double complex* x, blasint* incx)
#endif
{
	void (*fn) (void* n, void* x, void* incx);
	void (*fn_hook) (void* n, void* x, void* incx);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zlacgv.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zlacgv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) x, (void*) incx); 
		return;
	} else {
		hook_pos_zlacgv = 0;
		fn_hook((void*) n, (void*) x, (void*) incx);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlacgv_(blasint* n, double complex* x, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(zlacgv,ZLACGV)))));
#else
#ifndef __APPLE__
void zlacgv(blasint* n, double complex* x, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(zlacgv,ZLACGV)))));
#else
void zlacgv(blasint* n, double complex* x, blasint* incx){ FC_GLOBAL(zlacgv,ZLACGV)((void*) n, (void*) x, (void*) incx); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlacgv_(void* n, void* x, void* incx)
{
	void (*fn) (void* n, void* x, void* incx);

	*(void **) & fn = current_backend->lapack.zlacgv.f77_blas_function; 

		fn((void*) n, (void*) x, (void*) incx); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zlacgv(void* n, void* x, void* incx) __attribute__((alias("flexiblas_real_zlacgv_")));
#else
void flexiblas_real_zlacgv(void* n, void* x, void* incx){flexiblas_real_zlacgv_((void*) n, (void*) x, (void*) incx);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlacgv_(void* n, void* x, void* incx)
{
	void (*fn) (void* n, void* x, void* incx);
	void (*fn_hook) (void* n, void* x, void* incx);

	*(void **) &fn      = current_backend->lapack.zlacgv.f77_blas_function; 

    hook_pos_zlacgv ++;
    if( hook_pos_zlacgv < __flexiblas_hooks->zlacgv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlacgv.f77_hook_function[hook_pos_zlacgv];
        fn_hook((void*) n, (void*) x, (void*) incx);
    } else {
        hook_pos_zlacgv = 0;
		fn((void*) n, (void*) x, (void*) incx); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zlacgv(void* n, void* x, void* incx) __attribute__((alias("flexiblas_chain_zlacgv_")));
#else
void flexiblas_chain_zlacgv(void* n, void* x, void* incx){flexiblas_chain_zlacgv_((void*) n, (void*) x, (void*) incx);}
#endif



