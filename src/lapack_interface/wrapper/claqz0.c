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


static TLS_STORE uint8_t hook_pos_claqz0 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claqz0,CLAQZ0)(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* alpha, float complex* beta, float complex* q, blasint* ldq, float complex* z, blasint* ldz, float complex* work, blasint* lwork, float* rwork, blasint* rec, blasint* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz)
#else
void FC_GLOBAL(claqz0,CLAQZ0)(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* alpha, float complex* beta, float complex* q, blasint* ldq, float complex* z, blasint* ldz, float complex* work, blasint* lwork, float* rwork, blasint* rec, blasint* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz)
#endif
{
	void (*fn) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz);
	void (*fn_hook) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.claqz0.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->claqz0.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info, ( flexiblas_fortran_charlen_t ) len_wants, ( flexiblas_fortran_charlen_t ) len_wantq, ( flexiblas_fortran_charlen_t ) len_wantz); 
		return;
	} else {
		hook_pos_claqz0 = 0;
		fn_hook((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info, ( flexiblas_fortran_charlen_t ) len_wants, ( flexiblas_fortran_charlen_t ) len_wantq, ( flexiblas_fortran_charlen_t ) len_wantz);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void claqz0_(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* alpha, float complex* beta, float complex* q, blasint* ldq, float complex* z, blasint* ldz, float complex* work, blasint* lwork, float* rwork, blasint* rec, blasint* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz) __attribute__((alias(MTS(FC_GLOBAL(claqz0,CLAQZ0)))));
#else
#ifndef __APPLE__
void claqz0(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* alpha, float complex* beta, float complex* q, blasint* ldq, float complex* z, blasint* ldz, float complex* work, blasint* lwork, float* rwork, blasint* rec, blasint* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz) __attribute__((alias(MTS(FC_GLOBAL(claqz0,CLAQZ0)))));
#else
void claqz0(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* alpha, float complex* beta, float complex* q, blasint* ldq, float complex* z, blasint* ldz, float complex* work, blasint* lwork, float* rwork, blasint* rec, blasint* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz){ FC_GLOBAL(claqz0,CLAQZ0)((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info, (flexiblas_fortran_charlen_t) len_wants, (flexiblas_fortran_charlen_t) len_wantq, (flexiblas_fortran_charlen_t) len_wantz); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claqz0_(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz)
{
	void (*fn) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz);

	*(void **) & fn = current_backend->lapack.claqz0.f77_blas_function; 

		fn((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info, ( flexiblas_fortran_charlen_t ) len_wants, ( flexiblas_fortran_charlen_t ) len_wantq, ( flexiblas_fortran_charlen_t ) len_wantz); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_claqz0(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz) __attribute__((alias("flexiblas_real_claqz0_")));
#else
void flexiblas_real_claqz0(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz){flexiblas_real_claqz0_((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info, (flexiblas_fortran_charlen_t) len_wants, (flexiblas_fortran_charlen_t) len_wantq, (flexiblas_fortran_charlen_t) len_wantz);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_claqz0_(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz)
{
	void (*fn) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz);
	void (*fn_hook) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz);

	*(void **) &fn      = current_backend->lapack.claqz0.f77_blas_function; 

    hook_pos_claqz0 ++;
    if( hook_pos_claqz0 < __flexiblas_hooks->claqz0.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->claqz0.f77_hook_function[hook_pos_claqz0];
        fn_hook((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info, ( flexiblas_fortran_charlen_t ) len_wants, ( flexiblas_fortran_charlen_t ) len_wantq, ( flexiblas_fortran_charlen_t ) len_wantz);
    } else {
        hook_pos_claqz0 = 0;
		fn((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info, ( flexiblas_fortran_charlen_t ) len_wants, ( flexiblas_fortran_charlen_t ) len_wantq, ( flexiblas_fortran_charlen_t ) len_wantz); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_claqz0(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz) __attribute__((alias("flexiblas_chain_claqz0_")));
#else
void flexiblas_chain_claqz0(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz){flexiblas_chain_claqz0_((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) rec, (void*) info, (flexiblas_fortran_charlen_t) len_wants, (flexiblas_fortran_charlen_t) len_wantq, (flexiblas_fortran_charlen_t) len_wantz);}
#endif



