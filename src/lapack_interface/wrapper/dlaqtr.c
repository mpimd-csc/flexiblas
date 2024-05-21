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


static TLS_STORE uint8_t hook_pos_dlaqtr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlaqtr,DLAQTR)(blasint* ltran, blasint* lreal, blasint* n, double* t, blasint* ldt, double* b, double* w, double* scale, double* x, double* work, blasint* info)
#else
void FC_GLOBAL(dlaqtr,DLAQTR)(blasint* ltran, blasint* lreal, blasint* n, double* t, blasint* ldt, double* b, double* w, double* scale, double* x, double* work, blasint* info)
#endif
{
	void (*fn) (void* ltran, void* lreal, void* n, void* t, void* ldt, void* b, void* w, void* scale, void* x, void* work, void* info);
	void (*fn_hook) (void* ltran, void* lreal, void* n, void* t, void* ldt, void* b, void* w, void* scale, void* x, void* work, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlaqtr.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlaqtr.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) ltran, (void*) lreal, (void*) n, (void*) t, (void*) ldt, (void*) b, (void*) w, (void*) scale, (void*) x, (void*) work, (void*) info); 
		return;
	} else {
		hook_pos_dlaqtr = 0;
		fn_hook((void*) ltran, (void*) lreal, (void*) n, (void*) t, (void*) ldt, (void*) b, (void*) w, (void*) scale, (void*) x, (void*) work, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlaqtr_(blasint* ltran, blasint* lreal, blasint* n, double* t, blasint* ldt, double* b, double* w, double* scale, double* x, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlaqtr,DLAQTR)))));
#else
#ifndef __APPLE__
void dlaqtr(blasint* ltran, blasint* lreal, blasint* n, double* t, blasint* ldt, double* b, double* w, double* scale, double* x, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlaqtr,DLAQTR)))));
#else
void dlaqtr(blasint* ltran, blasint* lreal, blasint* n, double* t, blasint* ldt, double* b, double* w, double* scale, double* x, double* work, blasint* info){ FC_GLOBAL(dlaqtr,DLAQTR)((void*) ltran, (void*) lreal, (void*) n, (void*) t, (void*) ldt, (void*) b, (void*) w, (void*) scale, (void*) x, (void*) work, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlaqtr_(void* ltran, void* lreal, void* n, void* t, void* ldt, void* b, void* w, void* scale, void* x, void* work, void* info)
{
	void (*fn) (void* ltran, void* lreal, void* n, void* t, void* ldt, void* b, void* w, void* scale, void* x, void* work, void* info);

	*(void **) & fn = current_backend->lapack.dlaqtr.f77_blas_function; 

		fn((void*) ltran, (void*) lreal, (void*) n, (void*) t, (void*) ldt, (void*) b, (void*) w, (void*) scale, (void*) x, (void*) work, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlaqtr(void* ltran, void* lreal, void* n, void* t, void* ldt, void* b, void* w, void* scale, void* x, void* work, void* info) __attribute__((alias("flexiblas_real_dlaqtr_")));
#else
void flexiblas_real_dlaqtr(void* ltran, void* lreal, void* n, void* t, void* ldt, void* b, void* w, void* scale, void* x, void* work, void* info){flexiblas_real_dlaqtr_((void*) ltran, (void*) lreal, (void*) n, (void*) t, (void*) ldt, (void*) b, (void*) w, (void*) scale, (void*) x, (void*) work, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlaqtr_(void* ltran, void* lreal, void* n, void* t, void* ldt, void* b, void* w, void* scale, void* x, void* work, void* info)
{
	void (*fn) (void* ltran, void* lreal, void* n, void* t, void* ldt, void* b, void* w, void* scale, void* x, void* work, void* info);
	void (*fn_hook) (void* ltran, void* lreal, void* n, void* t, void* ldt, void* b, void* w, void* scale, void* x, void* work, void* info);

	*(void **) &fn      = current_backend->lapack.dlaqtr.f77_blas_function; 

    hook_pos_dlaqtr ++;
    if( hook_pos_dlaqtr < __flexiblas_hooks->dlaqtr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlaqtr.f77_hook_function[hook_pos_dlaqtr];
        fn_hook((void*) ltran, (void*) lreal, (void*) n, (void*) t, (void*) ldt, (void*) b, (void*) w, (void*) scale, (void*) x, (void*) work, (void*) info);
    } else {
        hook_pos_dlaqtr = 0;
		fn((void*) ltran, (void*) lreal, (void*) n, (void*) t, (void*) ldt, (void*) b, (void*) w, (void*) scale, (void*) x, (void*) work, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlaqtr(void* ltran, void* lreal, void* n, void* t, void* ldt, void* b, void* w, void* scale, void* x, void* work, void* info) __attribute__((alias("flexiblas_chain_dlaqtr_")));
#else
void flexiblas_chain_dlaqtr(void* ltran, void* lreal, void* n, void* t, void* ldt, void* b, void* w, void* scale, void* x, void* work, void* info){flexiblas_chain_dlaqtr_((void*) ltran, (void*) lreal, (void*) n, (void*) t, (void*) ldt, (void*) b, (void*) w, (void*) scale, (void*) x, (void*) work, (void*) info);}
#endif



