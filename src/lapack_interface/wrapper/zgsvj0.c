//  SPDX-License-Identifier: LGPL-3.0-or-later
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


static TLS_STORE uint8_t hook_pos_zgsvj0 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgsvj0,ZGSVJ0)(char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* d, double* sva, blasint* mv, double complex* v, blasint* ldv, double* eps, double* sfmin, double* tol, blasint* nsweep, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobv)
#else
void FC_GLOBAL(zgsvj0,ZGSVJ0)(char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* d, double* sva, blasint* mv, double complex* v, blasint* ldv, double* eps, double* sfmin, double* tol, blasint* nsweep, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobv)
#endif
{
    void (*fn) (void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobv);
    void (*fn_hook) (void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobv);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zgsvj0.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zgsvj0.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobv);
        return;
    } else {
        hook_pos_zgsvj0 = 0;
        fn_hook((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobv);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void zgsvj0_(char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* d, double* sva, blasint* mv, double complex* v, blasint* ldv, double* eps, double* sfmin, double* tol, blasint* nsweep, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobv) __attribute__((alias(MTS(FC_GLOBAL(zgsvj0,ZGSVJ0)))));
#else
#ifndef __APPLE__
void zgsvj0(char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* d, double* sva, blasint* mv, double complex* v, blasint* ldv, double* eps, double* sfmin, double* tol, blasint* nsweep, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobv) __attribute__((alias(MTS(FC_GLOBAL(zgsvj0,ZGSVJ0)))));
#else
void zgsvj0(char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* d, double* sva, blasint* mv, double complex* v, blasint* ldv, double* eps, double* sfmin, double* tol, blasint* nsweep, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobv){ FC_GLOBAL(zgsvj0,ZGSVJ0)((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobv); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgsvj0_(void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobv)
{
    void (*fn) (void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobv);

    *(void **) & fn = current_backend->lapack.zgsvj0.f77_blas_function;

    fn((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobv);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zgsvj0(void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobv) __attribute__((alias("flexiblas_real_zgsvj0_")));
#else
void flexiblas_real_zgsvj0(void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobv){flexiblas_real_zgsvj0_((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobv);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgsvj0_(void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobv)
{
    void (*fn) (void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobv);
    void (*fn_hook) (void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobv);

    *(void **) &fn      = current_backend->lapack.zgsvj0.f77_blas_function;

    hook_pos_zgsvj0 ++;
    if( hook_pos_zgsvj0 < __flexiblas_hooks->zgsvj0.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgsvj0.f77_hook_function[hook_pos_zgsvj0];
        fn_hook((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobv);
    } else {
        hook_pos_zgsvj0 = 0;
        fn((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobv);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zgsvj0(void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobv) __attribute__((alias("flexiblas_chain_zgsvj0_")));
#else
void flexiblas_chain_zgsvj0(void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobv){flexiblas_chain_zgsvj0_((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobv);}
#endif



