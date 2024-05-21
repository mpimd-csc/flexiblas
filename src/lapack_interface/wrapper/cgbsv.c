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


static TLS_STORE uint8_t hook_pos_cgbsv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgbsv,CGBSV)(blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float complex* ab, blasint* ldab, blasint* ipiv, float complex* b, blasint* ldb, blasint* info)
#else
void FC_GLOBAL(cgbsv,CGBSV)(blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float complex* ab, blasint* ldab, blasint* ipiv, float complex* b, blasint* ldb, blasint* info)
#endif
{
	void (*fn) (void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info);
	void (*fn_hook) (void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.cgbsv.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->cgbsv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info); 
		return;
	} else {
		hook_pos_cgbsv = 0;
		fn_hook((void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cgbsv_(blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float complex* ab, blasint* ldab, blasint* ipiv, float complex* b, blasint* ldb, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgbsv,CGBSV)))));
#else
#ifndef __APPLE__
void cgbsv(blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float complex* ab, blasint* ldab, blasint* ipiv, float complex* b, blasint* ldb, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgbsv,CGBSV)))));
#else
void cgbsv(blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float complex* ab, blasint* ldab, blasint* ipiv, float complex* b, blasint* ldb, blasint* info){ FC_GLOBAL(cgbsv,CGBSV)((void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgbsv_(void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info)
{
	void (*fn) (void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info);

	*(void **) & fn = current_backend->lapack.cgbsv.f77_blas_function; 

		fn((void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_cgbsv(void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info) __attribute__((alias("flexiblas_real_cgbsv_")));
#else
void flexiblas_real_cgbsv(void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info){flexiblas_real_cgbsv_((void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgbsv_(void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info)
{
	void (*fn) (void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info);
	void (*fn_hook) (void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info);

	*(void **) &fn      = current_backend->lapack.cgbsv.f77_blas_function; 

    hook_pos_cgbsv ++;
    if( hook_pos_cgbsv < __flexiblas_hooks->cgbsv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgbsv.f77_hook_function[hook_pos_cgbsv];
        fn_hook((void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info);
    } else {
        hook_pos_cgbsv = 0;
		fn((void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_cgbsv(void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info) __attribute__((alias("flexiblas_chain_cgbsv_")));
#else
void flexiblas_chain_cgbsv(void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info){flexiblas_chain_cgbsv_((void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info);}
#endif



