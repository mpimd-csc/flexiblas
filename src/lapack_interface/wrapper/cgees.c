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


static TLS_STORE uint8_t hook_pos_cgees = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgees,CGEES)(char* jobvs, char* sort, blasint* select, blasint* n, float complex* a, blasint* lda, blasint* sdim, float complex* w, float complex* vs, blasint* ldvs, float complex* work, blasint* lwork, float* rwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort)
#else
void FC_GLOBAL(cgees,CGEES)(char* jobvs, char* sort, blasint* select, blasint* n, float complex* a, blasint* lda, blasint* sdim, float complex* w, float complex* vs, blasint* ldvs, float complex* work, blasint* lwork, float* rwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort)
#endif
{
    void (*fn) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);
    void (*fn_hook) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.cgees.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->cgees.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort);
        return;
    } else {
        hook_pos_cgees = 0;
        fn_hook((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void cgees_(char* jobvs, char* sort, blasint* select, blasint* n, float complex* a, blasint* lda, blasint* sdim, float complex* w, float complex* vs, blasint* ldvs, float complex* work, blasint* lwork, float* rwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort) __attribute__((alias(MTS(FC_GLOBAL(cgees,CGEES)))));
#else
#ifndef __APPLE__
void cgees(char* jobvs, char* sort, blasint* select, blasint* n, float complex* a, blasint* lda, blasint* sdim, float complex* w, float complex* vs, blasint* ldvs, float complex* work, blasint* lwork, float* rwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort) __attribute__((alias(MTS(FC_GLOBAL(cgees,CGEES)))));
#else
void cgees(char* jobvs, char* sort, blasint* select, blasint* n, float complex* a, blasint* lda, blasint* sdim, float complex* w, float complex* vs, blasint* ldvs, float complex* work, blasint* lwork, float* rwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort){ FC_GLOBAL(cgees,CGEES)((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvs, (flexiblas_fortran_charlen_t) len_sort); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgees_(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort)
{
    void (*fn) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

    *(void **) & fn = current_backend->lapack.cgees.f77_blas_function;

    fn((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort);

    return;
}
#ifndef __APPLE__
void flexiblas_real_cgees(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort) __attribute__((alias("flexiblas_real_cgees_")));
#else
void flexiblas_real_cgees(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort){flexiblas_real_cgees_((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvs, (flexiblas_fortran_charlen_t) len_sort);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgees_(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort)
{
    void (*fn) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);
    void (*fn_hook) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

    *(void **) &fn      = current_backend->lapack.cgees.f77_blas_function;

    hook_pos_cgees ++;
    if( hook_pos_cgees < __flexiblas_hooks->cgees.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgees.f77_hook_function[hook_pos_cgees];
        fn_hook((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort);
    } else {
        hook_pos_cgees = 0;
        fn((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_cgees(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort) __attribute__((alias("flexiblas_chain_cgees_")));
#else
void flexiblas_chain_cgees(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort){flexiblas_chain_cgees_((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvs, (flexiblas_fortran_charlen_t) len_sort);}
#endif



