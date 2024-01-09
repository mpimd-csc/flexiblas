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



static TLS_STORE uint8_t hook_pos_slalsd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slalsd,SLALSD)(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, float* b, blasint* ldb, float* rcond, blasint* rank_bn, float* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(slalsd,SLALSD)(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, float* b, blasint* ldb, float* rcond, blasint* rank_bn, float* work, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* iwork, void* info);
	void (*fn_hook) (void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slalsd.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slalsd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_slalsd = 0;
		fn_hook((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slalsd_(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, float* b, blasint* ldb, float* rcond, blasint* rank_bn, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slalsd,SLALSD)))));
#else
#ifndef __APPLE__
void slalsd(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, float* b, blasint* ldb, float* rcond, blasint* rank_bn, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slalsd,SLALSD)))));
#else
void slalsd(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, float* b, blasint* ldb, float* rcond, blasint* rank_bn, float* work, blasint* iwork, blasint* info){ FC_GLOBAL(slalsd,SLALSD)((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) iwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slalsd_(void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* iwork, void* info)
{
	void (*fn) (void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* iwork, void* info);

	*(void **) & fn = current_backend->lapack.slalsd.f77_blas_function; 

		fn((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) iwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slalsd(void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_real_slalsd_")));
#else
void flexiblas_real_slalsd(void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* iwork, void* info){flexiblas_real_slalsd_((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slalsd_(void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* iwork, void* info)
{
	void (*fn) (void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* iwork, void* info);
	void (*fn_hook) (void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* iwork, void* info);

	*(void **) &fn      = current_backend->lapack.slalsd.f77_blas_function; 

    hook_pos_slalsd ++;
    if( hook_pos_slalsd < __flexiblas_hooks->slalsd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slalsd.f77_hook_function[hook_pos_slalsd];
        fn_hook((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_slalsd = 0;
		fn((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) iwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slalsd(void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_chain_slalsd_")));
#else
void flexiblas_chain_slalsd(void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* iwork, void* info){flexiblas_chain_slalsd_((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) iwork, (void*) info);}
#endif



