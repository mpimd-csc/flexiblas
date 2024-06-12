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


static TLS_STORE uint8_t hook_pos_spotrf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(spotrf,SPOTRF)(char* uplo, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(spotrf,SPOTRF)(char* uplo, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    void (*fn) (void* uplo, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.spotrf.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->spotrf.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    } else {
        hook_pos_spotrf = 0;
        fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void spotrf_(char* uplo, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(spotrf,SPOTRF)))));
#else
#ifndef __APPLE__
void spotrf(char* uplo, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(spotrf,SPOTRF)))));
#else
void spotrf(char* uplo, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(spotrf,SPOTRF)((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_spotrf_(void* uplo, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) & fn = current_backend->lapack.spotrf.f77_blas_function;

    fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);

    return;
}
#ifndef __APPLE__
void flexiblas_real_spotrf(void* uplo, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_spotrf_")));
#else
void flexiblas_real_spotrf(void* uplo, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_spotrf_((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_spotrf_(void* uplo, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) &fn      = current_backend->lapack.spotrf.f77_blas_function;

    hook_pos_spotrf ++;
    if( hook_pos_spotrf < __flexiblas_hooks->spotrf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->spotrf.f77_hook_function[hook_pos_spotrf];
        fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_spotrf = 0;
        fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_spotrf(void* uplo, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_spotrf_")));
#else
void flexiblas_chain_spotrf(void* uplo, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_spotrf_((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



