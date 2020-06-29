/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Wed Mar 28 11:20:04 2018 */
        
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "fortran_mangle.h"

#include "flexiblas.h"


#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_dtfsm = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dtfsm,DTFSM)(char* transr, char* side, char* uplo, char* trans, char* diag, blasint* m, blasint* n, double* alpha, double* a, double* b, blasint* ldb)
#else
void FC_GLOBAL(dtfsm,DTFSM)(char* transr, char* side, char* uplo, char* trans, char* diag, blasint* m, blasint* n, double* alpha, double* a, double* b, blasint* ldb)
#endif
{
	void (*fn) (void* transr, void* side, void* uplo, void* trans, void* diag, void* m, void* n, void* alpha, void* a, void* b, void* ldb);
	void (*fn_hook) (void* transr, void* side, void* uplo, void* trans, void* diag, void* m, void* n, void* alpha, void* a, void* b, void* ldb);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dtfsm.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dtfsm.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) transr, (void*) side, (void*) uplo, (void*) trans, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) b, (void*) ldb); 
		return;
	} else {
		hook_pos_dtfsm = 0;
		fn_hook((void*) transr, (void*) side, (void*) uplo, (void*) trans, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) b, (void*) ldb);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dtfsm_(char* transr, char* side, char* uplo, char* trans, char* diag, blasint* m, blasint* n, double* alpha, double* a, double* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(dtfsm,DTFSM)))));
#else
void dtfsm(char* transr, char* side, char* uplo, char* trans, char* diag, blasint* m, blasint* n, double* alpha, double* a, double* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(dtfsm,DTFSM)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dtfsm_(void* transr, void* side, void* uplo, void* trans, void* diag, void* m, void* n, void* alpha, void* a, void* b, void* ldb)
{
	void (*fn) (void* transr, void* side, void* uplo, void* trans, void* diag, void* m, void* n, void* alpha, void* a, void* b, void* ldb);

	fn = current_backend->lapack.dtfsm.f77_blas_function; 

		fn((void*) transr, (void*) side, (void*) uplo, (void*) trans, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) b, (void*) ldb); 

	return;
}

void flexiblas_real_dtfsm(void* transr, void* side, void* uplo, void* trans, void* diag, void* m, void* n, void* alpha, void* a, void* b, void* ldb)  __attribute__((alias("flexiblas_real_dtfsm_")));





/* Chainloader for Hooks */


void flexiblas_chain_dtfsm_(void* transr, void* side, void* uplo, void* trans, void* diag, void* m, void* n, void* alpha, void* a, void* b, void* ldb)
{
	void (*fn) (void* transr, void* side, void* uplo, void* trans, void* diag, void* m, void* n, void* alpha, void* a, void* b, void* ldb);
	void (*fn_hook) (void* transr, void* side, void* uplo, void* trans, void* diag, void* m, void* n, void* alpha, void* a, void* b, void* ldb);

	fn      = current_backend->lapack.dtfsm.f77_blas_function; 

    hook_pos_dtfsm ++;
    if( hook_pos_dtfsm < __flexiblas_hooks->dtfsm.nhook) {
        fn_hook = __flexiblas_hooks->dtfsm.f77_hook_function[hook_pos_dtfsm];
        fn_hook((void*) transr, (void*) side, (void*) uplo, (void*) trans, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) b, (void*) ldb);
    } else {
        hook_pos_dtfsm = 0;
		fn((void*) transr, (void*) side, (void*) uplo, (void*) trans, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) b, (void*) ldb); 
	}
	return;
}

void flexiblas_chain_dtfsm(void* transr, void* side, void* uplo, void* trans, void* diag, void* m, void* n, void* alpha, void* a, void* b, void* ldb)  __attribute__((alias("flexiblas_chain_dtfsm_")));




