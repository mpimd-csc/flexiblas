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


static TLS_STORE uint8_t hook_pos_sptsvx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sptsvx,SPTSVX)(char* fact, blasint* n, blasint* nrhs, float* d, float* e, float* df, float* ef, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* info, flexiblas_fortran_charlen_t len_fact)
#else
void FC_GLOBAL(sptsvx,SPTSVX)(char* fact, blasint* n, blasint* nrhs, float* d, float* e, float* df, float* ef, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* info, flexiblas_fortran_charlen_t len_fact)
#endif
{
	void (*fn) (void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact);
	void (*fn_hook) (void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.sptsvx.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->sptsvx.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact); 
		return;
	} else {
		hook_pos_sptsvx = 0;
		fn_hook((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sptsvx_(char* fact, blasint* n, blasint* nrhs, float* d, float* e, float* df, float* ef, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* info, flexiblas_fortran_charlen_t len_fact) __attribute__((alias(MTS(FC_GLOBAL(sptsvx,SPTSVX)))));
#else
#ifndef __APPLE__
void sptsvx(char* fact, blasint* n, blasint* nrhs, float* d, float* e, float* df, float* ef, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* info, flexiblas_fortran_charlen_t len_fact) __attribute__((alias(MTS(FC_GLOBAL(sptsvx,SPTSVX)))));
#else
void sptsvx(char* fact, blasint* n, blasint* nrhs, float* d, float* e, float* df, float* ef, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* info, flexiblas_fortran_charlen_t len_fact){ FC_GLOBAL(sptsvx,SPTSVX)((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_fact); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sptsvx_(void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact)
{
	void (*fn) (void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact);

	*(void **) & fn = current_backend->lapack.sptsvx.f77_blas_function; 

		fn((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_sptsvx(void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact) __attribute__((alias("flexiblas_real_sptsvx_")));
#else
void flexiblas_real_sptsvx(void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact){flexiblas_real_sptsvx_((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_fact);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sptsvx_(void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact)
{
	void (*fn) (void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact);
	void (*fn_hook) (void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact);

	*(void **) &fn      = current_backend->lapack.sptsvx.f77_blas_function; 

    hook_pos_sptsvx ++;
    if( hook_pos_sptsvx < __flexiblas_hooks->sptsvx.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sptsvx.f77_hook_function[hook_pos_sptsvx];
        fn_hook((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact);
    } else {
        hook_pos_sptsvx = 0;
		fn((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_sptsvx(void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact) __attribute__((alias("flexiblas_chain_sptsvx_")));
#else
void flexiblas_chain_sptsvx(void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact){flexiblas_chain_sptsvx_((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_fact);}
#endif



