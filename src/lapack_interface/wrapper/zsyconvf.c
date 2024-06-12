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


static TLS_STORE uint8_t hook_pos_zsyconvf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zsyconvf,ZSYCONVF)(char* uplo, char* way, blasint* n, double complex* a, blasint* lda, double complex* e, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way)
#else
void FC_GLOBAL(zsyconvf,ZSYCONVF)(char* uplo, char* way, blasint* n, double complex* a, blasint* lda, double complex* e, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way)
#endif
{
    void (*fn) (void* uplo, void* way, void* n, void* a, void* lda, void* e, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);
    void (*fn_hook) (void* uplo, void* way, void* n, void* a, void* lda, void* e, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zsyconvf.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zsyconvf.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_way);
        return;
    } else {
        hook_pos_zsyconvf = 0;
        fn_hook((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_way);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void zsyconvf_(char* uplo, char* way, blasint* n, double complex* a, blasint* lda, double complex* e, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way) __attribute__((alias(MTS(FC_GLOBAL(zsyconvf,ZSYCONVF)))));
#else
#ifndef __APPLE__
void zsyconvf(char* uplo, char* way, blasint* n, double complex* a, blasint* lda, double complex* e, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way) __attribute__((alias(MTS(FC_GLOBAL(zsyconvf,ZSYCONVF)))));
#else
void zsyconvf(char* uplo, char* way, blasint* n, double complex* a, blasint* lda, double complex* e, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way){ FC_GLOBAL(zsyconvf,ZSYCONVF)((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_way); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zsyconvf_(void* uplo, void* way, void* n, void* a, void* lda, void* e, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way)
{
    void (*fn) (void* uplo, void* way, void* n, void* a, void* lda, void* e, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);

    *(void **) & fn = current_backend->lapack.zsyconvf.f77_blas_function;

    fn((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_way);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zsyconvf(void* uplo, void* way, void* n, void* a, void* lda, void* e, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way) __attribute__((alias("flexiblas_real_zsyconvf_")));
#else
void flexiblas_real_zsyconvf(void* uplo, void* way, void* n, void* a, void* lda, void* e, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way){flexiblas_real_zsyconvf_((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_way);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zsyconvf_(void* uplo, void* way, void* n, void* a, void* lda, void* e, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way)
{
    void (*fn) (void* uplo, void* way, void* n, void* a, void* lda, void* e, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);
    void (*fn_hook) (void* uplo, void* way, void* n, void* a, void* lda, void* e, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);

    *(void **) &fn      = current_backend->lapack.zsyconvf.f77_blas_function;

    hook_pos_zsyconvf ++;
    if( hook_pos_zsyconvf < __flexiblas_hooks->zsyconvf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zsyconvf.f77_hook_function[hook_pos_zsyconvf];
        fn_hook((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_way);
    } else {
        hook_pos_zsyconvf = 0;
        fn((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_way);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zsyconvf(void* uplo, void* way, void* n, void* a, void* lda, void* e, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way) __attribute__((alias("flexiblas_chain_zsyconvf_")));
#else
void flexiblas_chain_zsyconvf(void* uplo, void* way, void* n, void* a, void* lda, void* e, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way){flexiblas_chain_zsyconvf_((void*) uplo, (void*) way, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_way);}
#endif



