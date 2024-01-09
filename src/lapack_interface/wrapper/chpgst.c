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


#if __GNUC__ > 7
typedef size_t fortran_charlen_t;
#else
typedef int fortran_charlen_t;
#endif

#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_chpgst = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(chpgst,CHPGST)(blasint* itype, char* uplo, blasint* n, float complex* ap, float complex* bp, blasint* info)
#else
void FC_GLOBAL(chpgst,CHPGST)(blasint* itype, char* uplo, blasint* n, float complex* ap, float complex* bp, blasint* info)
#endif
{
	void (*fn) (void* itype, void* uplo, void* n, void* ap, void* bp, void* info);
	void (*fn_hook) (void* itype, void* uplo, void* n, void* ap, void* bp, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.chpgst.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->chpgst.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) itype, (void*) uplo, (void*) n, (void*) ap, (void*) bp, (void*) info); 
		return;
	} else {
		hook_pos_chpgst = 0;
		fn_hook((void*) itype, (void*) uplo, (void*) n, (void*) ap, (void*) bp, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void chpgst_(blasint* itype, char* uplo, blasint* n, float complex* ap, float complex* bp, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(chpgst,CHPGST)))));
#else
#ifndef __APPLE__
void chpgst(blasint* itype, char* uplo, blasint* n, float complex* ap, float complex* bp, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(chpgst,CHPGST)))));
#else
void chpgst(blasint* itype, char* uplo, blasint* n, float complex* ap, float complex* bp, blasint* info){ FC_GLOBAL(chpgst,CHPGST)((void*) itype, (void*) uplo, (void*) n, (void*) ap, (void*) bp, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_chpgst_(void* itype, void* uplo, void* n, void* ap, void* bp, void* info)
{
	void (*fn) (void* itype, void* uplo, void* n, void* ap, void* bp, void* info);

	*(void **) & fn = current_backend->lapack.chpgst.f77_blas_function; 

		fn((void*) itype, (void*) uplo, (void*) n, (void*) ap, (void*) bp, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_chpgst(void* itype, void* uplo, void* n, void* ap, void* bp, void* info) __attribute__((alias("flexiblas_real_chpgst_")));
#else
void flexiblas_real_chpgst(void* itype, void* uplo, void* n, void* ap, void* bp, void* info){flexiblas_real_chpgst_((void*) itype, (void*) uplo, (void*) n, (void*) ap, (void*) bp, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_chpgst_(void* itype, void* uplo, void* n, void* ap, void* bp, void* info)
{
	void (*fn) (void* itype, void* uplo, void* n, void* ap, void* bp, void* info);
	void (*fn_hook) (void* itype, void* uplo, void* n, void* ap, void* bp, void* info);

	*(void **) &fn      = current_backend->lapack.chpgst.f77_blas_function; 

    hook_pos_chpgst ++;
    if( hook_pos_chpgst < __flexiblas_hooks->chpgst.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->chpgst.f77_hook_function[hook_pos_chpgst];
        fn_hook((void*) itype, (void*) uplo, (void*) n, (void*) ap, (void*) bp, (void*) info);
    } else {
        hook_pos_chpgst = 0;
		fn((void*) itype, (void*) uplo, (void*) n, (void*) ap, (void*) bp, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_chpgst(void* itype, void* uplo, void* n, void* ap, void* bp, void* info) __attribute__((alias("flexiblas_chain_chpgst_")));
#else
void flexiblas_chain_chpgst(void* itype, void* uplo, void* n, void* ap, void* bp, void* info){flexiblas_chain_chpgst_((void*) itype, (void*) uplo, (void*) n, (void*) ap, (void*) bp, (void*) info);}
#endif



