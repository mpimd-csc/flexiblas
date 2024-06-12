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


static TLS_STORE uint8_t hook_pos_sgegs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgegs,SGEGS)(char* jobvsl, char* jobvsr, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr)
#else
void FC_GLOBAL(sgegs,SGEGS)(char* jobvsl, char* jobvsr, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr)
#endif
{
    void (*fn) (void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);
    void (*fn_hook) (void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.sgegs.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->sgegs.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr);
        return;
    } else {
        hook_pos_sgegs = 0;
        fn_hook((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void sgegs_(char* jobvsl, char* jobvsr, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr) __attribute__((alias(MTS(FC_GLOBAL(sgegs,SGEGS)))));
#else
#ifndef __APPLE__
void sgegs(char* jobvsl, char* jobvsr, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr) __attribute__((alias(MTS(FC_GLOBAL(sgegs,SGEGS)))));
#else
void sgegs(char* jobvsl, char* jobvsr, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr){ FC_GLOBAL(sgegs,SGEGS)((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgegs_(void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr)
{
    void (*fn) (void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);

    *(void **) & fn = current_backend->lapack.sgegs.f77_blas_function;

    fn((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr);

    return;
}
#ifndef __APPLE__
void flexiblas_real_sgegs(void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr) __attribute__((alias("flexiblas_real_sgegs_")));
#else
void flexiblas_real_sgegs(void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr){flexiblas_real_sgegs_((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sgegs_(void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr)
{
    void (*fn) (void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);
    void (*fn_hook) (void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);

    *(void **) &fn      = current_backend->lapack.sgegs.f77_blas_function;

    hook_pos_sgegs ++;
    if( hook_pos_sgegs < __flexiblas_hooks->sgegs.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sgegs.f77_hook_function[hook_pos_sgegs];
        fn_hook((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr);
    } else {
        hook_pos_sgegs = 0;
        fn((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_sgegs(void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr) __attribute__((alias("flexiblas_chain_sgegs_")));
#else
void flexiblas_chain_sgegs(void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr){flexiblas_chain_sgegs_((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr);}
#endif



