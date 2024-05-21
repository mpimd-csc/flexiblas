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


static TLS_STORE uint8_t hook_pos_sgesvx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgesvx,SGESVX)(char* fact, char* trans, blasint* n, blasint* nrhs, float* a, blasint* lda, float* af, blasint* ldaf, blasint* ipiv, char* equed, float* r, float* c, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed)
#else
void FC_GLOBAL(sgesvx,SGESVX)(char* fact, char* trans, blasint* n, blasint* nrhs, float* a, blasint* lda, float* af, blasint* ldaf, blasint* ipiv, char* equed, float* r, float* c, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed)
#endif
{
	void (*fn) (void* fact, void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);
	void (*fn_hook) (void* fact, void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.sgesvx.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->sgesvx.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) fact, (void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_equed); 
		return;
	} else {
		hook_pos_sgesvx = 0;
		fn_hook((void*) fact, (void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_equed);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sgesvx_(char* fact, char* trans, blasint* n, blasint* nrhs, float* a, blasint* lda, float* af, blasint* ldaf, blasint* ipiv, char* equed, float* r, float* c, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed) __attribute__((alias(MTS(FC_GLOBAL(sgesvx,SGESVX)))));
#else
#ifndef __APPLE__
void sgesvx(char* fact, char* trans, blasint* n, blasint* nrhs, float* a, blasint* lda, float* af, blasint* ldaf, blasint* ipiv, char* equed, float* r, float* c, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed) __attribute__((alias(MTS(FC_GLOBAL(sgesvx,SGESVX)))));
#else
void sgesvx(char* fact, char* trans, blasint* n, blasint* nrhs, float* a, blasint* lda, float* af, blasint* ldaf, blasint* ipiv, char* equed, float* r, float* c, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed){ FC_GLOBAL(sgesvx,SGESVX)((void*) fact, (void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_fact, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_equed); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgesvx_(void* fact, void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed)
{
	void (*fn) (void* fact, void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

	*(void **) & fn = current_backend->lapack.sgesvx.f77_blas_function; 

		fn((void*) fact, (void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_equed); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_sgesvx(void* fact, void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed) __attribute__((alias("flexiblas_real_sgesvx_")));
#else
void flexiblas_real_sgesvx(void* fact, void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed){flexiblas_real_sgesvx_((void*) fact, (void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_fact, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_equed);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sgesvx_(void* fact, void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed)
{
	void (*fn) (void* fact, void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);
	void (*fn_hook) (void* fact, void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

	*(void **) &fn      = current_backend->lapack.sgesvx.f77_blas_function; 

    hook_pos_sgesvx ++;
    if( hook_pos_sgesvx < __flexiblas_hooks->sgesvx.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sgesvx.f77_hook_function[hook_pos_sgesvx];
        fn_hook((void*) fact, (void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_equed);
    } else {
        hook_pos_sgesvx = 0;
		fn((void*) fact, (void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_equed); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_sgesvx(void* fact, void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed) __attribute__((alias("flexiblas_chain_sgesvx_")));
#else
void flexiblas_chain_sgesvx(void* fact, void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed){flexiblas_chain_sgesvx_((void*) fact, (void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_fact, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_equed);}
#endif



