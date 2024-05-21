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


static TLS_STORE uint8_t hook_pos_dgghd3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dgghd3,DGGHD3)(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz)
#else
void FC_GLOBAL(dgghd3,DGGHD3)(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz)
#endif
{
	void (*fn) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);
	void (*fn_hook) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dgghd3.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dgghd3.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_compq, ( flexiblas_fortran_charlen_t ) len_compz); 
		return;
	} else {
		hook_pos_dgghd3 = 0;
		fn_hook((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_compq, ( flexiblas_fortran_charlen_t ) len_compz);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dgghd3_(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz) __attribute__((alias(MTS(FC_GLOBAL(dgghd3,DGGHD3)))));
#else
#ifndef __APPLE__
void dgghd3(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz) __attribute__((alias(MTS(FC_GLOBAL(dgghd3,DGGHD3)))));
#else
void dgghd3(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz){ FC_GLOBAL(dgghd3,DGGHD3)((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_compq, (flexiblas_fortran_charlen_t) len_compz); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dgghd3_(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz)
{
	void (*fn) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

	*(void **) & fn = current_backend->lapack.dgghd3.f77_blas_function; 

		fn((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_compq, ( flexiblas_fortran_charlen_t ) len_compz); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dgghd3(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz) __attribute__((alias("flexiblas_real_dgghd3_")));
#else
void flexiblas_real_dgghd3(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz){flexiblas_real_dgghd3_((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_compq, (flexiblas_fortran_charlen_t) len_compz);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dgghd3_(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz)
{
	void (*fn) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);
	void (*fn_hook) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

	*(void **) &fn      = current_backend->lapack.dgghd3.f77_blas_function; 

    hook_pos_dgghd3 ++;
    if( hook_pos_dgghd3 < __flexiblas_hooks->dgghd3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dgghd3.f77_hook_function[hook_pos_dgghd3];
        fn_hook((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_compq, ( flexiblas_fortran_charlen_t ) len_compz);
    } else {
        hook_pos_dgghd3 = 0;
		fn((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_compq, ( flexiblas_fortran_charlen_t ) len_compz); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dgghd3(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz) __attribute__((alias("flexiblas_chain_dgghd3_")));
#else
void flexiblas_chain_dgghd3(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz){flexiblas_chain_dgghd3_((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_compq, (flexiblas_fortran_charlen_t) len_compz);}
#endif



