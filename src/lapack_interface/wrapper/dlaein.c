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


static TLS_STORE uint8_t hook_pos_dlaein = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlaein,DLAEIN)(blasint* rightv, blasint* noinit, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vr, double* vi, double* b, blasint* ldb, double* work, double* eps3, double* smlnum, double* bignum, blasint* info)
#else
void FC_GLOBAL(dlaein,DLAEIN)(blasint* rightv, blasint* noinit, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vr, double* vi, double* b, blasint* ldb, double* work, double* eps3, double* smlnum, double* bignum, blasint* info)
#endif
{
	void (*fn) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* wr, void* wi, void* vr, void* vi, void* b, void* ldb, void* work, void* eps3, void* smlnum, void* bignum, void* info);
	void (*fn_hook) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* wr, void* wi, void* vr, void* vi, void* b, void* ldb, void* work, void* eps3, void* smlnum, void* bignum, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlaein.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlaein.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vr, (void*) vi, (void*) b, (void*) ldb, (void*) work, (void*) eps3, (void*) smlnum, (void*) bignum, (void*) info); 
		return;
	} else {
		hook_pos_dlaein = 0;
		fn_hook((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vr, (void*) vi, (void*) b, (void*) ldb, (void*) work, (void*) eps3, (void*) smlnum, (void*) bignum, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlaein_(blasint* rightv, blasint* noinit, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vr, double* vi, double* b, blasint* ldb, double* work, double* eps3, double* smlnum, double* bignum, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlaein,DLAEIN)))));
#else
#ifndef __APPLE__
void dlaein(blasint* rightv, blasint* noinit, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vr, double* vi, double* b, blasint* ldb, double* work, double* eps3, double* smlnum, double* bignum, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlaein,DLAEIN)))));
#else
void dlaein(blasint* rightv, blasint* noinit, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vr, double* vi, double* b, blasint* ldb, double* work, double* eps3, double* smlnum, double* bignum, blasint* info){ FC_GLOBAL(dlaein,DLAEIN)((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vr, (void*) vi, (void*) b, (void*) ldb, (void*) work, (void*) eps3, (void*) smlnum, (void*) bignum, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlaein_(void* rightv, void* noinit, void* n, void* h, void* ldh, void* wr, void* wi, void* vr, void* vi, void* b, void* ldb, void* work, void* eps3, void* smlnum, void* bignum, void* info)
{
	void (*fn) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* wr, void* wi, void* vr, void* vi, void* b, void* ldb, void* work, void* eps3, void* smlnum, void* bignum, void* info);

	*(void **) & fn = current_backend->lapack.dlaein.f77_blas_function; 

		fn((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vr, (void*) vi, (void*) b, (void*) ldb, (void*) work, (void*) eps3, (void*) smlnum, (void*) bignum, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlaein(void* rightv, void* noinit, void* n, void* h, void* ldh, void* wr, void* wi, void* vr, void* vi, void* b, void* ldb, void* work, void* eps3, void* smlnum, void* bignum, void* info) __attribute__((alias("flexiblas_real_dlaein_")));
#else
void flexiblas_real_dlaein(void* rightv, void* noinit, void* n, void* h, void* ldh, void* wr, void* wi, void* vr, void* vi, void* b, void* ldb, void* work, void* eps3, void* smlnum, void* bignum, void* info){flexiblas_real_dlaein_((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vr, (void*) vi, (void*) b, (void*) ldb, (void*) work, (void*) eps3, (void*) smlnum, (void*) bignum, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlaein_(void* rightv, void* noinit, void* n, void* h, void* ldh, void* wr, void* wi, void* vr, void* vi, void* b, void* ldb, void* work, void* eps3, void* smlnum, void* bignum, void* info)
{
	void (*fn) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* wr, void* wi, void* vr, void* vi, void* b, void* ldb, void* work, void* eps3, void* smlnum, void* bignum, void* info);
	void (*fn_hook) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* wr, void* wi, void* vr, void* vi, void* b, void* ldb, void* work, void* eps3, void* smlnum, void* bignum, void* info);

	*(void **) &fn      = current_backend->lapack.dlaein.f77_blas_function; 

    hook_pos_dlaein ++;
    if( hook_pos_dlaein < __flexiblas_hooks->dlaein.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlaein.f77_hook_function[hook_pos_dlaein];
        fn_hook((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vr, (void*) vi, (void*) b, (void*) ldb, (void*) work, (void*) eps3, (void*) smlnum, (void*) bignum, (void*) info);
    } else {
        hook_pos_dlaein = 0;
		fn((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vr, (void*) vi, (void*) b, (void*) ldb, (void*) work, (void*) eps3, (void*) smlnum, (void*) bignum, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlaein(void* rightv, void* noinit, void* n, void* h, void* ldh, void* wr, void* wi, void* vr, void* vi, void* b, void* ldb, void* work, void* eps3, void* smlnum, void* bignum, void* info) __attribute__((alias("flexiblas_chain_dlaein_")));
#else
void flexiblas_chain_dlaein(void* rightv, void* noinit, void* n, void* h, void* ldh, void* wr, void* wi, void* vr, void* vi, void* b, void* ldb, void* work, void* eps3, void* smlnum, void* bignum, void* info){flexiblas_chain_dlaein_((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vr, (void*) vi, (void*) b, (void*) ldb, (void*) work, (void*) eps3, (void*) smlnum, (void*) bignum, (void*) info);}
#endif



