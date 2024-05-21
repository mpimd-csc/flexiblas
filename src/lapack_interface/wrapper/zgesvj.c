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


static TLS_STORE uint8_t hook_pos_zgesvj = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgesvj,ZGESVJ)(char* joba, char* jobu, char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double* sva, blasint* mv, double complex* v, blasint* ldv, double complex* cwork, blasint* lwork, double* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv)
#else
void FC_GLOBAL(zgesvj,ZGESVJ)(char* joba, char* jobu, char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double* sva, blasint* mv, double complex* v, blasint* ldv, double complex* cwork, blasint* lwork, double* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv)
#endif
{
	void (*fn) (void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* cwork, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv);
	void (*fn_hook) (void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* cwork, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zgesvj.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zgesvj.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) cwork, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_joba, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv); 
		return;
	} else {
		hook_pos_zgesvj = 0;
		fn_hook((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) cwork, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_joba, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zgesvj_(char* joba, char* jobu, char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double* sva, blasint* mv, double complex* v, blasint* ldv, double complex* cwork, blasint* lwork, double* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv) __attribute__((alias(MTS(FC_GLOBAL(zgesvj,ZGESVJ)))));
#else
#ifndef __APPLE__
void zgesvj(char* joba, char* jobu, char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double* sva, blasint* mv, double complex* v, blasint* ldv, double complex* cwork, blasint* lwork, double* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv) __attribute__((alias(MTS(FC_GLOBAL(zgesvj,ZGESVJ)))));
#else
void zgesvj(char* joba, char* jobu, char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double* sva, blasint* mv, double complex* v, blasint* ldv, double complex* cwork, blasint* lwork, double* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv){ FC_GLOBAL(zgesvj,ZGESVJ)((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) cwork, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_joba, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobv); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgesvj_(void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* cwork, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv)
{
	void (*fn) (void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* cwork, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv);

	*(void **) & fn = current_backend->lapack.zgesvj.f77_blas_function; 

		fn((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) cwork, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_joba, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zgesvj(void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* cwork, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv) __attribute__((alias("flexiblas_real_zgesvj_")));
#else
void flexiblas_real_zgesvj(void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* cwork, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv){flexiblas_real_zgesvj_((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) cwork, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_joba, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobv);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgesvj_(void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* cwork, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv)
{
	void (*fn) (void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* cwork, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv);
	void (*fn_hook) (void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* cwork, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv);

	*(void **) &fn      = current_backend->lapack.zgesvj.f77_blas_function; 

    hook_pos_zgesvj ++;
    if( hook_pos_zgesvj < __flexiblas_hooks->zgesvj.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgesvj.f77_hook_function[hook_pos_zgesvj];
        fn_hook((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) cwork, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_joba, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv);
    } else {
        hook_pos_zgesvj = 0;
		fn((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) cwork, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_joba, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zgesvj(void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* cwork, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv) __attribute__((alias("flexiblas_chain_zgesvj_")));
#else
void flexiblas_chain_zgesvj(void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* cwork, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv){flexiblas_chain_zgesvj_((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) cwork, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_joba, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobv);}
#endif



