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


static TLS_STORE uint8_t hook_pos_sgesc2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgesc2,SGESC2)(blasint* n, float* a, blasint* lda, float* rhs, blasint* ipiv, blasint* jpiv, float* scale)
#else
void FC_GLOBAL(sgesc2,SGESC2)(blasint* n, float* a, blasint* lda, float* rhs, blasint* ipiv, blasint* jpiv, float* scale)
#endif
{
    void (*fn) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);
    void (*fn_hook) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.sgesc2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->sgesc2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);
        return;
    } else {
        hook_pos_sgesc2 = 0;
        fn_hook((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(sgesc2,SGESC2)(blasint* n, float* a, blasint* lda, float* rhs, blasint* ipiv, blasint* jpiv, float* scale) __attribute__((alias(MTS(FC_GLOBAL(sgesc2,SGESC2)))));
void FC_GLOBAL3(sgesc2,SGESC2)(blasint* n, float* a, blasint* lda, float* rhs, blasint* ipiv, blasint* jpiv, float* scale) __attribute__((alias(MTS(FC_GLOBAL(sgesc2,SGESC2)))));
#else
void FC_GLOBAL2(sgesc2,SGESC2)(blasint* n, float* a, blasint* lda, float* rhs, blasint* ipiv, blasint* jpiv, float* scale){ FC_GLOBAL(sgesc2,SGESC2)((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale); }
void FC_GLOBAL3(sgesc2,SGESC2)(blasint* n, float* a, blasint* lda, float* rhs, blasint* ipiv, blasint* jpiv, float* scale){ FC_GLOBAL(sgesc2,SGESC2)((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgesc2_(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale)
{
    void (*fn) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);

    *(void **) & fn = current_backend->lapack.sgesc2.f77_blas_function;

    fn((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);

    return;
}
#ifndef __APPLE__
void flexiblas_real_sgesc2(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale) __attribute__((alias("flexiblas_real_sgesc2_")));
#else
void flexiblas_real_sgesc2(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale){flexiblas_real_sgesc2_((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sgesc2_(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale)
{
    void (*fn) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);
    void (*fn_hook) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);

    *(void **) &fn      = current_backend->lapack.sgesc2.f77_blas_function;

    hook_pos_sgesc2 ++;
    if( hook_pos_sgesc2 < __flexiblas_hooks->sgesc2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sgesc2.f77_hook_function[hook_pos_sgesc2];
        fn_hook((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);
    } else {
        hook_pos_sgesc2 = 0;
        fn((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_sgesc2(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale) __attribute__((alias("flexiblas_chain_sgesc2_")));
#else
void flexiblas_chain_sgesc2(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale){flexiblas_chain_sgesc2_((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);}
#endif



