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


static TLS_STORE uint8_t hook_pos_dsgesv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dsgesv,DSGESV)(blasint* n, blasint* nrhs, double* a, blasint* lda, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info)
#else
void FC_GLOBAL(dsgesv,DSGESV)(blasint* n, blasint* nrhs, double* a, blasint* lda, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info)
#endif
{
	void (*fn) (void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info);
	void (*fn_hook) (void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dsgesv.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dsgesv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info); 
		return;
	} else {
		hook_pos_dsgesv = 0;
		fn_hook((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dsgesv_(blasint* n, blasint* nrhs, double* a, blasint* lda, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dsgesv,DSGESV)))));
#else
#ifndef __APPLE__
void dsgesv(blasint* n, blasint* nrhs, double* a, blasint* lda, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dsgesv,DSGESV)))));
#else
void dsgesv(blasint* n, blasint* nrhs, double* a, blasint* lda, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info){ FC_GLOBAL(dsgesv,DSGESV)((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dsgesv_(void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info)
{
	void (*fn) (void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info);

	*(void **) & fn = current_backend->lapack.dsgesv.f77_blas_function; 

		fn((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dsgesv(void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info) __attribute__((alias("flexiblas_real_dsgesv_")));
#else
void flexiblas_real_dsgesv(void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info){flexiblas_real_dsgesv_((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dsgesv_(void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info)
{
	void (*fn) (void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info);
	void (*fn_hook) (void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info);

	*(void **) &fn      = current_backend->lapack.dsgesv.f77_blas_function; 

    hook_pos_dsgesv ++;
    if( hook_pos_dsgesv < __flexiblas_hooks->dsgesv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dsgesv.f77_hook_function[hook_pos_dsgesv];
        fn_hook((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info);
    } else {
        hook_pos_dsgesv = 0;
		fn((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dsgesv(void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info) __attribute__((alias("flexiblas_chain_dsgesv_")));
#else
void flexiblas_chain_dsgesv(void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info){flexiblas_chain_dsgesv_((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info);}
#endif



