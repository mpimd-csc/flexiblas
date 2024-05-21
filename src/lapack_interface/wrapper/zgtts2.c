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


static TLS_STORE uint8_t hook_pos_zgtts2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgtts2,ZGTTS2)(blasint* itrans, blasint* n, blasint* nrhs, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, double complex* b, blasint* ldb)
#else
void FC_GLOBAL(zgtts2,ZGTTS2)(blasint* itrans, blasint* n, blasint* nrhs, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, double complex* b, blasint* ldb)
#endif
{
	void (*fn) (void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb);
	void (*fn_hook) (void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zgtts2.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zgtts2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb); 
		return;
	} else {
		hook_pos_zgtts2 = 0;
		fn_hook((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zgtts2_(blasint* itrans, blasint* n, blasint* nrhs, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, double complex* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(zgtts2,ZGTTS2)))));
#else
#ifndef __APPLE__
void zgtts2(blasint* itrans, blasint* n, blasint* nrhs, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, double complex* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(zgtts2,ZGTTS2)))));
#else
void zgtts2(blasint* itrans, blasint* n, blasint* nrhs, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, double complex* b, blasint* ldb){ FC_GLOBAL(zgtts2,ZGTTS2)((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgtts2_(void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb)
{
	void (*fn) (void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb);

	*(void **) & fn = current_backend->lapack.zgtts2.f77_blas_function; 

		fn((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zgtts2(void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb) __attribute__((alias("flexiblas_real_zgtts2_")));
#else
void flexiblas_real_zgtts2(void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb){flexiblas_real_zgtts2_((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgtts2_(void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb)
{
	void (*fn) (void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb);
	void (*fn_hook) (void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb);

	*(void **) &fn      = current_backend->lapack.zgtts2.f77_blas_function; 

    hook_pos_zgtts2 ++;
    if( hook_pos_zgtts2 < __flexiblas_hooks->zgtts2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgtts2.f77_hook_function[hook_pos_zgtts2];
        fn_hook((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb);
    } else {
        hook_pos_zgtts2 = 0;
		fn((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zgtts2(void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb) __attribute__((alias("flexiblas_chain_zgtts2_")));
#else
void flexiblas_chain_zgtts2(void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb){flexiblas_chain_zgtts2_((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb);}
#endif



