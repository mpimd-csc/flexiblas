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


static TLS_STORE uint8_t hook_pos_strrfs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(strrfs,STRRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag)
#else
void FC_GLOBAL(strrfs,STRRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag)
#endif
{
    void (*fn) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);
    void (*fn_hook) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.strrfs.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->strrfs.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag);
        return;
    } else {
        hook_pos_strrfs = 0;
        fn_hook((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void strrfs_(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(strrfs,STRRFS)))));
#else
#ifndef __APPLE__
void strrfs(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(strrfs,STRRFS)))));
#else
void strrfs(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag){ FC_GLOBAL(strrfs,STRRFS)((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_strrfs_(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag)
{
    void (*fn) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    *(void **) & fn = current_backend->lapack.strrfs.f77_blas_function;

    fn((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag);

    return;
}
#ifndef __APPLE__
void flexiblas_real_strrfs(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_real_strrfs_")));
#else
void flexiblas_real_strrfs(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag){flexiblas_real_strrfs_((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_strrfs_(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag)
{
    void (*fn) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);
    void (*fn_hook) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    *(void **) &fn      = current_backend->lapack.strrfs.f77_blas_function;

    hook_pos_strrfs ++;
    if( hook_pos_strrfs < __flexiblas_hooks->strrfs.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->strrfs.f77_hook_function[hook_pos_strrfs];
        fn_hook((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag);
    } else {
        hook_pos_strrfs = 0;
        fn((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_strrfs(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_chain_strrfs_")));
#else
void flexiblas_chain_strrfs(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag){flexiblas_chain_strrfs_((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag);}
#endif



