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


static TLS_STORE uint8_t hook_pos_zlahef = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlahef,ZLAHEF)(char* uplo, blasint* n, blasint* nb, blasint* kb, double complex* a, blasint* lda, blasint* ipiv, double complex* w, blasint* ldw, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(zlahef,ZLAHEF)(char* uplo, blasint* n, blasint* nb, blasint* kb, double complex* a, blasint* lda, blasint* ipiv, double complex* w, blasint* ldw, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    void (*fn) (void* uplo, void* n, void* nb, void* kb, void* a, void* lda, void* ipiv, void* w, void* ldw, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* nb, void* kb, void* a, void* lda, void* ipiv, void* w, void* ldw, void* info, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlahef.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlahef.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) n, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) ipiv, (void*) w, (void*) ldw, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    } else {
        hook_pos_zlahef = 0;
        fn_hook((void*) uplo, (void*) n, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) ipiv, (void*) w, (void*) ldw, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void zlahef_(char* uplo, blasint* n, blasint* nb, blasint* kb, double complex* a, blasint* lda, blasint* ipiv, double complex* w, blasint* ldw, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(zlahef,ZLAHEF)))));
#else
#ifndef __APPLE__
void zlahef(char* uplo, blasint* n, blasint* nb, blasint* kb, double complex* a, blasint* lda, blasint* ipiv, double complex* w, blasint* ldw, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(zlahef,ZLAHEF)))));
#else
void zlahef(char* uplo, blasint* n, blasint* nb, blasint* kb, double complex* a, blasint* lda, blasint* ipiv, double complex* w, blasint* ldw, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(zlahef,ZLAHEF)((void*) uplo, (void*) n, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) ipiv, (void*) w, (void*) ldw, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlahef_(void* uplo, void* n, void* nb, void* kb, void* a, void* lda, void* ipiv, void* w, void* ldw, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* nb, void* kb, void* a, void* lda, void* ipiv, void* w, void* ldw, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) & fn = current_backend->lapack.zlahef.f77_blas_function;

    fn((void*) uplo, (void*) n, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) ipiv, (void*) w, (void*) ldw, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlahef(void* uplo, void* n, void* nb, void* kb, void* a, void* lda, void* ipiv, void* w, void* ldw, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_zlahef_")));
#else
void flexiblas_real_zlahef(void* uplo, void* n, void* nb, void* kb, void* a, void* lda, void* ipiv, void* w, void* ldw, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_zlahef_((void*) uplo, (void*) n, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) ipiv, (void*) w, (void*) ldw, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlahef_(void* uplo, void* n, void* nb, void* kb, void* a, void* lda, void* ipiv, void* w, void* ldw, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* nb, void* kb, void* a, void* lda, void* ipiv, void* w, void* ldw, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* nb, void* kb, void* a, void* lda, void* ipiv, void* w, void* ldw, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) &fn      = current_backend->lapack.zlahef.f77_blas_function;

    hook_pos_zlahef ++;
    if( hook_pos_zlahef < __flexiblas_hooks->zlahef.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlahef.f77_hook_function[hook_pos_zlahef];
        fn_hook((void*) uplo, (void*) n, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) ipiv, (void*) w, (void*) ldw, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_zlahef = 0;
        fn((void*) uplo, (void*) n, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) ipiv, (void*) w, (void*) ldw, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlahef(void* uplo, void* n, void* nb, void* kb, void* a, void* lda, void* ipiv, void* w, void* ldw, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_zlahef_")));
#else
void flexiblas_chain_zlahef(void* uplo, void* n, void* nb, void* kb, void* a, void* lda, void* ipiv, void* w, void* ldw, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_zlahef_((void*) uplo, (void*) n, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) ipiv, (void*) w, (void*) ldw, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



