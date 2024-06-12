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


static TLS_STORE uint8_t hook_pos_cpbrfs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cpbrfs,CPBRFS)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, float complex* ab, blasint* ldab, float complex* afb, blasint* ldafb, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(cpbrfs,CPBRFS)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, float complex* ab, blasint* ldab, float complex* afb, blasint* ldafb, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    void (*fn) (void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.cpbrfs.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->cpbrfs.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    } else {
        hook_pos_cpbrfs = 0;
        fn_hook((void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void cpbrfs_(char* uplo, blasint* n, blasint* kd, blasint* nrhs, float complex* ab, blasint* ldab, float complex* afb, blasint* ldafb, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(cpbrfs,CPBRFS)))));
#else
#ifndef __APPLE__
void cpbrfs(char* uplo, blasint* n, blasint* kd, blasint* nrhs, float complex* ab, blasint* ldab, float complex* afb, blasint* ldafb, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(cpbrfs,CPBRFS)))));
#else
void cpbrfs(char* uplo, blasint* n, blasint* kd, blasint* nrhs, float complex* ab, blasint* ldab, float complex* afb, blasint* ldafb, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(cpbrfs,CPBRFS)((void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cpbrfs_(void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) & fn = current_backend->lapack.cpbrfs.f77_blas_function;

    fn((void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);

    return;
}
#ifndef __APPLE__
void flexiblas_real_cpbrfs(void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_cpbrfs_")));
#else
void flexiblas_real_cpbrfs(void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_cpbrfs_((void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cpbrfs_(void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) &fn      = current_backend->lapack.cpbrfs.f77_blas_function;

    hook_pos_cpbrfs ++;
    if( hook_pos_cpbrfs < __flexiblas_hooks->cpbrfs.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cpbrfs.f77_hook_function[hook_pos_cpbrfs];
        fn_hook((void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_cpbrfs = 0;
        fn((void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_cpbrfs(void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_cpbrfs_")));
#else
void flexiblas_chain_cpbrfs(void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_cpbrfs_((void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



