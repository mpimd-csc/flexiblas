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


static TLS_STORE uint8_t hook_pos_dptts2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dptts2,DPTTS2)(blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb)
#else
void FC_GLOBAL(dptts2,DPTTS2)(blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb)
#endif
{
	void (*fn) (void* n, void* nrhs, void* d, void* e, void* b, void* ldb);
	void (*fn_hook) (void* n, void* nrhs, void* d, void* e, void* b, void* ldb);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dptts2.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dptts2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb); 
		return;
	} else {
		hook_pos_dptts2 = 0;
		fn_hook((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dptts2_(blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(dptts2,DPTTS2)))));
#else
#ifndef __APPLE__
void dptts2(blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(dptts2,DPTTS2)))));
#else
void dptts2(blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb){ FC_GLOBAL(dptts2,DPTTS2)((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dptts2_(void* n, void* nrhs, void* d, void* e, void* b, void* ldb)
{
	void (*fn) (void* n, void* nrhs, void* d, void* e, void* b, void* ldb);

	*(void **) & fn = current_backend->lapack.dptts2.f77_blas_function; 

		fn((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dptts2(void* n, void* nrhs, void* d, void* e, void* b, void* ldb) __attribute__((alias("flexiblas_real_dptts2_")));
#else
void flexiblas_real_dptts2(void* n, void* nrhs, void* d, void* e, void* b, void* ldb){flexiblas_real_dptts2_((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dptts2_(void* n, void* nrhs, void* d, void* e, void* b, void* ldb)
{
	void (*fn) (void* n, void* nrhs, void* d, void* e, void* b, void* ldb);
	void (*fn_hook) (void* n, void* nrhs, void* d, void* e, void* b, void* ldb);

	*(void **) &fn      = current_backend->lapack.dptts2.f77_blas_function; 

    hook_pos_dptts2 ++;
    if( hook_pos_dptts2 < __flexiblas_hooks->dptts2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dptts2.f77_hook_function[hook_pos_dptts2];
        fn_hook((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb);
    } else {
        hook_pos_dptts2 = 0;
		fn((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dptts2(void* n, void* nrhs, void* d, void* e, void* b, void* ldb) __attribute__((alias("flexiblas_chain_dptts2_")));
#else
void flexiblas_chain_dptts2(void* n, void* nrhs, void* d, void* e, void* b, void* ldb){flexiblas_chain_dptts2_((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb);}
#endif



