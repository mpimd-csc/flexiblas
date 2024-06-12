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


static TLS_STORE uint8_t hook_pos_zpstf2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zpstf2,ZPSTF2)(char* uplo, blasint* n, double complex* a, blasint* lda, blasint* piv, blasint* rank_bn, double* tol, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(zpstf2,ZPSTF2)(char* uplo, blasint* n, double complex* a, blasint* lda, blasint* piv, blasint* rank_bn, double* tol, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    void (*fn) (void* uplo, void* n, void* a, void* lda, void* piv, void* rank_bn, void* tol, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* piv, void* rank_bn, void* tol, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zpstf2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zpstf2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) piv, (void*) rank_bn, (void*) tol, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    } else {
        hook_pos_zpstf2 = 0;
        fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) piv, (void*) rank_bn, (void*) tol, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void zpstf2_(char* uplo, blasint* n, double complex* a, blasint* lda, blasint* piv, blasint* rank_bn, double* tol, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(zpstf2,ZPSTF2)))));
#else
#ifndef __APPLE__
void zpstf2(char* uplo, blasint* n, double complex* a, blasint* lda, blasint* piv, blasint* rank_bn, double* tol, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(zpstf2,ZPSTF2)))));
#else
void zpstf2(char* uplo, blasint* n, double complex* a, blasint* lda, blasint* piv, blasint* rank_bn, double* tol, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(zpstf2,ZPSTF2)((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) piv, (void*) rank_bn, (void*) tol, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zpstf2_(void* uplo, void* n, void* a, void* lda, void* piv, void* rank_bn, void* tol, void* work, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* a, void* lda, void* piv, void* rank_bn, void* tol, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) & fn = current_backend->lapack.zpstf2.f77_blas_function;

    fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) piv, (void*) rank_bn, (void*) tol, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zpstf2(void* uplo, void* n, void* a, void* lda, void* piv, void* rank_bn, void* tol, void* work, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_zpstf2_")));
#else
void flexiblas_real_zpstf2(void* uplo, void* n, void* a, void* lda, void* piv, void* rank_bn, void* tol, void* work, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_zpstf2_((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) piv, (void*) rank_bn, (void*) tol, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zpstf2_(void* uplo, void* n, void* a, void* lda, void* piv, void* rank_bn, void* tol, void* work, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* a, void* lda, void* piv, void* rank_bn, void* tol, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* piv, void* rank_bn, void* tol, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) &fn      = current_backend->lapack.zpstf2.f77_blas_function;

    hook_pos_zpstf2 ++;
    if( hook_pos_zpstf2 < __flexiblas_hooks->zpstf2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zpstf2.f77_hook_function[hook_pos_zpstf2];
        fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) piv, (void*) rank_bn, (void*) tol, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_zpstf2 = 0;
        fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) piv, (void*) rank_bn, (void*) tol, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zpstf2(void* uplo, void* n, void* a, void* lda, void* piv, void* rank_bn, void* tol, void* work, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_zpstf2_")));
#else
void flexiblas_chain_zpstf2(void* uplo, void* n, void* a, void* lda, void* piv, void* rank_bn, void* tol, void* work, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_zpstf2_((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) piv, (void*) rank_bn, (void*) tol, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



