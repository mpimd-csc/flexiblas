//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2025 Martin Koehler

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_config.h"

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_dgges3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dgges3,DGGES3)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, blasint* sdim, double* alphar, double* alphai, double* beta, double* vsl, blasint* ldvsl, double* vsr, blasint* ldvsr, double* work, blasint* lwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort)
#else
void FC_GLOBAL(dgges3,DGGES3)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, blasint* sdim, double* alphar, double* alphai, double* beta, double* vsl, blasint* ldvsl, double* vsr, blasint* ldvsr, double* work, blasint* lwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort)
#endif
{
    void (*fn) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort);
    void (*fn_hook) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dgges3.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dgges3.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr, ( flexiblas_fortran_charlen_t ) len_sort);
        return;
    } else {
        hook_pos_dgges3 = 0;
        fn_hook((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr, ( flexiblas_fortran_charlen_t ) len_sort);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dgges3,DGGES3)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, blasint* sdim, double* alphar, double* alphai, double* beta, double* vsl, blasint* ldvsl, double* vsr, blasint* ldvsr, double* work, blasint* lwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort) __attribute__((alias(MTS(FC_GLOBAL(dgges3,DGGES3)))));
void FC_GLOBAL3(dgges3,DGGES3)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, blasint* sdim, double* alphar, double* alphai, double* beta, double* vsl, blasint* ldvsl, double* vsr, blasint* ldvsr, double* work, blasint* lwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort) __attribute__((alias(MTS(FC_GLOBAL(dgges3,DGGES3)))));
#else
void FC_GLOBAL2(dgges3,DGGES3)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, blasint* sdim, double* alphar, double* alphai, double* beta, double* vsl, blasint* ldvsl, double* vsr, blasint* ldvsr, double* work, blasint* lwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort){ FC_GLOBAL(dgges3,DGGES3)((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr, (flexiblas_fortran_charlen_t) len_sort); }
void FC_GLOBAL3(dgges3,DGGES3)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, blasint* sdim, double* alphar, double* alphai, double* beta, double* vsl, blasint* ldvsl, double* vsr, blasint* ldvsr, double* work, blasint* lwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort){ FC_GLOBAL(dgges3,DGGES3)((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr, (flexiblas_fortran_charlen_t) len_sort); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dgges3_(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort)
{
    void (*fn) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort);

    *(void **) & fn = current_backend->lapack.dgges3.f77_blas_function;

    fn((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr, ( flexiblas_fortran_charlen_t ) len_sort);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dgges3(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort) __attribute__((alias("flexiblas_real_dgges3_")));
#else
void flexiblas_real_dgges3(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort){flexiblas_real_dgges3_((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr, (flexiblas_fortran_charlen_t) len_sort);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dgges3_(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort)
{
    void (*fn) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort);
    void (*fn_hook) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort);

    *(void **) &fn      = current_backend->lapack.dgges3.f77_blas_function;

    hook_pos_dgges3 ++;
    if( hook_pos_dgges3 < __flexiblas_hooks->dgges3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dgges3.f77_hook_function[hook_pos_dgges3];
        fn_hook((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr, ( flexiblas_fortran_charlen_t ) len_sort);
    } else {
        hook_pos_dgges3 = 0;
        fn((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr, ( flexiblas_fortran_charlen_t ) len_sort);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dgges3(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort) __attribute__((alias("flexiblas_chain_dgges3_")));
#else
void flexiblas_chain_dgges3(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort){flexiblas_chain_dgges3_((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr, (flexiblas_fortran_charlen_t) len_sort);}
#endif



