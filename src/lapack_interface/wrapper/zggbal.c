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


static TLS_STORE uint8_t hook_pos_zggbal = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zggbal,ZGGBAL)(char* job, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* work, blasint* info, flexiblas_fortran_charlen_t len_job)
#else
void FC_GLOBAL(zggbal,ZGGBAL)(char* job, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* work, blasint* info, flexiblas_fortran_charlen_t len_job)
#endif
{
    void (*fn) (void* job, void* n, void* a, void* lda, void* b, void* ldb, void* ilo, void* ihi, void* lscale, void* rscale, void* work, void* info, flexiblas_fortran_charlen_t len_job);
    void (*fn_hook) (void* job, void* n, void* a, void* lda, void* b, void* ldb, void* ilo, void* ihi, void* lscale, void* rscale, void* work, void* info, flexiblas_fortran_charlen_t len_job);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zggbal.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zggbal.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) job, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_job);
        return;
    } else {
        hook_pos_zggbal = 0;
        fn_hook((void*) job, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_job);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void zggbal_(char* job, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* work, blasint* info, flexiblas_fortran_charlen_t len_job) __attribute__((alias(MTS(FC_GLOBAL(zggbal,ZGGBAL)))));
#else
#ifndef __APPLE__
void zggbal(char* job, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* work, blasint* info, flexiblas_fortran_charlen_t len_job) __attribute__((alias(MTS(FC_GLOBAL(zggbal,ZGGBAL)))));
#else
void zggbal(char* job, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* work, blasint* info, flexiblas_fortran_charlen_t len_job){ FC_GLOBAL(zggbal,ZGGBAL)((void*) job, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_job); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zggbal_(void* job, void* n, void* a, void* lda, void* b, void* ldb, void* ilo, void* ihi, void* lscale, void* rscale, void* work, void* info, flexiblas_fortran_charlen_t len_job)
{
    void (*fn) (void* job, void* n, void* a, void* lda, void* b, void* ldb, void* ilo, void* ihi, void* lscale, void* rscale, void* work, void* info, flexiblas_fortran_charlen_t len_job);

    *(void **) & fn = current_backend->lapack.zggbal.f77_blas_function;

    fn((void*) job, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_job);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zggbal(void* job, void* n, void* a, void* lda, void* b, void* ldb, void* ilo, void* ihi, void* lscale, void* rscale, void* work, void* info, flexiblas_fortran_charlen_t len_job) __attribute__((alias("flexiblas_real_zggbal_")));
#else
void flexiblas_real_zggbal(void* job, void* n, void* a, void* lda, void* b, void* ldb, void* ilo, void* ihi, void* lscale, void* rscale, void* work, void* info, flexiblas_fortran_charlen_t len_job){flexiblas_real_zggbal_((void*) job, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_job);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zggbal_(void* job, void* n, void* a, void* lda, void* b, void* ldb, void* ilo, void* ihi, void* lscale, void* rscale, void* work, void* info, flexiblas_fortran_charlen_t len_job)
{
    void (*fn) (void* job, void* n, void* a, void* lda, void* b, void* ldb, void* ilo, void* ihi, void* lscale, void* rscale, void* work, void* info, flexiblas_fortran_charlen_t len_job);
    void (*fn_hook) (void* job, void* n, void* a, void* lda, void* b, void* ldb, void* ilo, void* ihi, void* lscale, void* rscale, void* work, void* info, flexiblas_fortran_charlen_t len_job);

    *(void **) &fn      = current_backend->lapack.zggbal.f77_blas_function;

    hook_pos_zggbal ++;
    if( hook_pos_zggbal < __flexiblas_hooks->zggbal.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zggbal.f77_hook_function[hook_pos_zggbal];
        fn_hook((void*) job, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_job);
    } else {
        hook_pos_zggbal = 0;
        fn((void*) job, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_job);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zggbal(void* job, void* n, void* a, void* lda, void* b, void* ldb, void* ilo, void* ihi, void* lscale, void* rscale, void* work, void* info, flexiblas_fortran_charlen_t len_job) __attribute__((alias("flexiblas_chain_zggbal_")));
#else
void flexiblas_chain_zggbal(void* job, void* n, void* a, void* lda, void* b, void* ldb, void* ilo, void* ihi, void* lscale, void* rscale, void* work, void* info, flexiblas_fortran_charlen_t len_job){flexiblas_chain_zggbal_((void*) job, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_job);}
#endif



