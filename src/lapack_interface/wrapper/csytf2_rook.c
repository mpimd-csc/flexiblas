//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2025 Martin Koehler

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_config.h"

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_csytf2_rook = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL_(csytf2_rook,CSYTF2_ROOK)(char* uplo, blasint* n, float complex* a, blasint* lda, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL_(csytf2_rook,CSYTF2_ROOK)(char* uplo, blasint* n, float complex* a, blasint* lda, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    void (*fn) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.csytf2_rook.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->csytf2_rook.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    } else {
        hook_pos_csytf2_rook = 0;
        fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2_(csytf2_rook,CSYTF2_ROOK)(char* uplo, blasint* n, float complex* a, blasint* lda, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL_(csytf2_rook,CSYTF2_ROOK)))));
void FC_GLOBAL3_(csytf2_rook,CSYTF2_ROOK)(char* uplo, blasint* n, float complex* a, blasint* lda, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL_(csytf2_rook,CSYTF2_ROOK)))));
#else
void FC_GLOBAL2_(csytf2_rook,CSYTF2_ROOK)(char* uplo, blasint* n, float complex* a, blasint* lda, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL_(csytf2_rook,CSYTF2_ROOK)((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
void FC_GLOBAL3_(csytf2_rook,CSYTF2_ROOK)(char* uplo, blasint* n, float complex* a, blasint* lda, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL_(csytf2_rook,CSYTF2_ROOK)((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_csytf2_rook_(void* uplo, void* n, void* a, void* lda, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) & fn = current_backend->lapack.csytf2_rook.f77_blas_function;

    fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);

    return;
}
#ifndef __APPLE__
void flexiblas_real_csytf2_rook(void* uplo, void* n, void* a, void* lda, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_csytf2_rook_")));
#else
void flexiblas_real_csytf2_rook(void* uplo, void* n, void* a, void* lda, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_csytf2_rook_((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_csytf2_rook_(void* uplo, void* n, void* a, void* lda, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) &fn      = current_backend->lapack.csytf2_rook.f77_blas_function;

    hook_pos_csytf2_rook ++;
    if( hook_pos_csytf2_rook < __flexiblas_hooks->csytf2_rook.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->csytf2_rook.f77_hook_function[hook_pos_csytf2_rook];
        fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_csytf2_rook = 0;
        fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_csytf2_rook(void* uplo, void* n, void* a, void* lda, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_csytf2_rook_")));
#else
void flexiblas_chain_csytf2_rook(void* uplo, void* n, void* a, void* lda, void* ipiv, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_csytf2_rook_((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



