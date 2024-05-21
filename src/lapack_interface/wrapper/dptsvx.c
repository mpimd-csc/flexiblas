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


static TLS_STORE uint8_t hook_pos_dptsvx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dptsvx,DPTSVX)(char* fact, blasint* n, blasint* nrhs, double* d, double* e, double* df, double* ef, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* info, flexiblas_fortran_charlen_t len_fact)
#else
void FC_GLOBAL(dptsvx,DPTSVX)(char* fact, blasint* n, blasint* nrhs, double* d, double* e, double* df, double* ef, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* info, flexiblas_fortran_charlen_t len_fact)
#endif
{
	void (*fn) (void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact);
	void (*fn_hook) (void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dptsvx.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dptsvx.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact); 
		return;
	} else {
		hook_pos_dptsvx = 0;
		fn_hook((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dptsvx_(char* fact, blasint* n, blasint* nrhs, double* d, double* e, double* df, double* ef, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* info, flexiblas_fortran_charlen_t len_fact) __attribute__((alias(MTS(FC_GLOBAL(dptsvx,DPTSVX)))));
#else
#ifndef __APPLE__
void dptsvx(char* fact, blasint* n, blasint* nrhs, double* d, double* e, double* df, double* ef, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* info, flexiblas_fortran_charlen_t len_fact) __attribute__((alias(MTS(FC_GLOBAL(dptsvx,DPTSVX)))));
#else
void dptsvx(char* fact, blasint* n, blasint* nrhs, double* d, double* e, double* df, double* ef, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* info, flexiblas_fortran_charlen_t len_fact){ FC_GLOBAL(dptsvx,DPTSVX)((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_fact); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dptsvx_(void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact)
{
	void (*fn) (void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact);

	*(void **) & fn = current_backend->lapack.dptsvx.f77_blas_function; 

		fn((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dptsvx(void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact) __attribute__((alias("flexiblas_real_dptsvx_")));
#else
void flexiblas_real_dptsvx(void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact){flexiblas_real_dptsvx_((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_fact);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dptsvx_(void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact)
{
	void (*fn) (void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact);
	void (*fn_hook) (void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact);

	*(void **) &fn      = current_backend->lapack.dptsvx.f77_blas_function; 

    hook_pos_dptsvx ++;
    if( hook_pos_dptsvx < __flexiblas_hooks->dptsvx.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dptsvx.f77_hook_function[hook_pos_dptsvx];
        fn_hook((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact);
    } else {
        hook_pos_dptsvx = 0;
		fn((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dptsvx(void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact) __attribute__((alias("flexiblas_chain_dptsvx_")));
#else
void flexiblas_chain_dptsvx(void* fact, void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* info, flexiblas_fortran_charlen_t len_fact){flexiblas_chain_dptsvx_((void*) fact, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_fact);}
#endif



