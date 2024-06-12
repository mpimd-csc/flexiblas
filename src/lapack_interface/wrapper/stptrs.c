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


static TLS_STORE uint8_t hook_pos_stptrs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(stptrs,STPTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* ap, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag)
#else
void FC_GLOBAL(stptrs,STPTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* ap, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag)
#endif
{
    void (*fn) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);
    void (*fn_hook) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.stptrs.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->stptrs.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag);
        return;
    } else {
        hook_pos_stptrs = 0;
        fn_hook((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void stptrs_(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* ap, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(stptrs,STPTRS)))));
#else
#ifndef __APPLE__
void stptrs(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* ap, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(stptrs,STPTRS)))));
#else
void stptrs(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* ap, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag){ FC_GLOBAL(stptrs,STPTRS)((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_stptrs_(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag)
{
    void (*fn) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    *(void **) & fn = current_backend->lapack.stptrs.f77_blas_function;

    fn((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag);

    return;
}
#ifndef __APPLE__
void flexiblas_real_stptrs(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_real_stptrs_")));
#else
void flexiblas_real_stptrs(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag){flexiblas_real_stptrs_((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_stptrs_(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag)
{
    void (*fn) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);
    void (*fn_hook) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    *(void **) &fn      = current_backend->lapack.stptrs.f77_blas_function;

    hook_pos_stptrs ++;
    if( hook_pos_stptrs < __flexiblas_hooks->stptrs.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->stptrs.f77_hook_function[hook_pos_stptrs];
        fn_hook((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag);
    } else {
        hook_pos_stptrs = 0;
        fn((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_stptrs(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_chain_stptrs_")));
#else
void flexiblas_chain_stptrs(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag){flexiblas_chain_stptrs_((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag);}
#endif



