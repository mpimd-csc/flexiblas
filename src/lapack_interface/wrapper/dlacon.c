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


static TLS_STORE uint8_t hook_pos_dlacon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlacon,DLACON)(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase)
#else
void FC_GLOBAL(dlacon,DLACON)(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase)
#endif
{
	void (*fn) (void* n, void* v, void* x, void* isgn, void* est, void* kase);
	void (*fn_hook) (void* n, void* v, void* x, void* isgn, void* est, void* kase);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlacon.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlacon.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase); 
		return;
	} else {
		hook_pos_dlacon = 0;
		fn_hook((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlacon_(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase) __attribute__((alias(MTS(FC_GLOBAL(dlacon,DLACON)))));
#else
#ifndef __APPLE__
void dlacon(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase) __attribute__((alias(MTS(FC_GLOBAL(dlacon,DLACON)))));
#else
void dlacon(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase){ FC_GLOBAL(dlacon,DLACON)((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlacon_(void* n, void* v, void* x, void* isgn, void* est, void* kase)
{
	void (*fn) (void* n, void* v, void* x, void* isgn, void* est, void* kase);

	*(void **) & fn = current_backend->lapack.dlacon.f77_blas_function; 

		fn((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlacon(void* n, void* v, void* x, void* isgn, void* est, void* kase) __attribute__((alias("flexiblas_real_dlacon_")));
#else
void flexiblas_real_dlacon(void* n, void* v, void* x, void* isgn, void* est, void* kase){flexiblas_real_dlacon_((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlacon_(void* n, void* v, void* x, void* isgn, void* est, void* kase)
{
	void (*fn) (void* n, void* v, void* x, void* isgn, void* est, void* kase);
	void (*fn_hook) (void* n, void* v, void* x, void* isgn, void* est, void* kase);

	*(void **) &fn      = current_backend->lapack.dlacon.f77_blas_function; 

    hook_pos_dlacon ++;
    if( hook_pos_dlacon < __flexiblas_hooks->dlacon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlacon.f77_hook_function[hook_pos_dlacon];
        fn_hook((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase);
    } else {
        hook_pos_dlacon = 0;
		fn((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlacon(void* n, void* v, void* x, void* isgn, void* est, void* kase) __attribute__((alias("flexiblas_chain_dlacon_")));
#else
void flexiblas_chain_dlacon(void* n, void* v, void* x, void* isgn, void* est, void* kase){flexiblas_chain_dlacon_((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase);}
#endif



