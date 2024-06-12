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


static TLS_STORE uint8_t hook_pos_ssyconv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ssyconv,SSYCONV)(char* uplo, char* way, blasint* n, float* a, blasint* lda, blasint* ipiv, float* e, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way)
#else
void FC_GLOBAL(ssyconv,SSYCONV)(char* uplo, char* way, blasint* n, float* a, blasint* lda, blasint* ipiv, float* e, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way)
#endif
{
    void (*fn) (void* uplo, void* way, void* n, void* a, void* lda, void* ipiv, void* e, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);
    void (*fn_hook) (void* uplo, void* way, void* n, void* a, void* lda, void* ipiv, void* e, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ssyconv.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ssyconv.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) e, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_way);
        return;
    } else {
        hook_pos_ssyconv = 0;
        fn_hook((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) e, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_way);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void ssyconv_(char* uplo, char* way, blasint* n, float* a, blasint* lda, blasint* ipiv, float* e, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way) __attribute__((alias(MTS(FC_GLOBAL(ssyconv,SSYCONV)))));
#else
#ifndef __APPLE__
void ssyconv(char* uplo, char* way, blasint* n, float* a, blasint* lda, blasint* ipiv, float* e, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way) __attribute__((alias(MTS(FC_GLOBAL(ssyconv,SSYCONV)))));
#else
void ssyconv(char* uplo, char* way, blasint* n, float* a, blasint* lda, blasint* ipiv, float* e, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way){ FC_GLOBAL(ssyconv,SSYCONV)((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) e, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_way); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ssyconv_(void* uplo, void* way, void* n, void* a, void* lda, void* ipiv, void* e, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way)
{
    void (*fn) (void* uplo, void* way, void* n, void* a, void* lda, void* ipiv, void* e, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);

    *(void **) & fn = current_backend->lapack.ssyconv.f77_blas_function;

    fn((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) e, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_way);

    return;
}
#ifndef __APPLE__
void flexiblas_real_ssyconv(void* uplo, void* way, void* n, void* a, void* lda, void* ipiv, void* e, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way) __attribute__((alias("flexiblas_real_ssyconv_")));
#else
void flexiblas_real_ssyconv(void* uplo, void* way, void* n, void* a, void* lda, void* ipiv, void* e, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way){flexiblas_real_ssyconv_((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) e, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_way);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ssyconv_(void* uplo, void* way, void* n, void* a, void* lda, void* ipiv, void* e, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way)
{
    void (*fn) (void* uplo, void* way, void* n, void* a, void* lda, void* ipiv, void* e, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);
    void (*fn_hook) (void* uplo, void* way, void* n, void* a, void* lda, void* ipiv, void* e, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);

    *(void **) &fn      = current_backend->lapack.ssyconv.f77_blas_function;

    hook_pos_ssyconv ++;
    if( hook_pos_ssyconv < __flexiblas_hooks->ssyconv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ssyconv.f77_hook_function[hook_pos_ssyconv];
        fn_hook((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) e, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_way);
    } else {
        hook_pos_ssyconv = 0;
        fn((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) e, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_way);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_ssyconv(void* uplo, void* way, void* n, void* a, void* lda, void* ipiv, void* e, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way) __attribute__((alias("flexiblas_chain_ssyconv_")));
#else
void flexiblas_chain_ssyconv(void* uplo, void* way, void* n, void* a, void* lda, void* ipiv, void* e, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way){flexiblas_chain_ssyconv_((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) e, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_way);}
#endif



