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


static TLS_STORE uint8_t hook_pos_zlaswp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlaswp,ZLASWP)(blasint* n, double complex* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx)
#else
void FC_GLOBAL(zlaswp,ZLASWP)(blasint* n, double complex* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx)
#endif
{
    void (*fn) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);
    void (*fn_hook) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlaswp.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlaswp.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);
        return;
    } else {
        hook_pos_zlaswp = 0;
        fn_hook((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zlaswp,ZLASWP)(blasint* n, double complex* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(zlaswp,ZLASWP)))));
void FC_GLOBAL3(zlaswp,ZLASWP)(blasint* n, double complex* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(zlaswp,ZLASWP)))));
#else
void FC_GLOBAL2(zlaswp,ZLASWP)(blasint* n, double complex* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx){ FC_GLOBAL(zlaswp,ZLASWP)((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx); }
void FC_GLOBAL3(zlaswp,ZLASWP)(blasint* n, double complex* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx){ FC_GLOBAL(zlaswp,ZLASWP)((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaswp_(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx)
{
    void (*fn) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);

    *(void **) & fn = current_backend->lapack.zlaswp.f77_blas_function;

    fn((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlaswp(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx) __attribute__((alias("flexiblas_real_zlaswp_")));
#else
void flexiblas_real_zlaswp(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx){flexiblas_real_zlaswp_((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlaswp_(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx)
{
    void (*fn) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);
    void (*fn_hook) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);

    *(void **) &fn      = current_backend->lapack.zlaswp.f77_blas_function;

    hook_pos_zlaswp ++;
    if( hook_pos_zlaswp < __flexiblas_hooks->zlaswp.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlaswp.f77_hook_function[hook_pos_zlaswp];
        fn_hook((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);
    } else {
        hook_pos_zlaswp = 0;
        fn((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlaswp(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx) __attribute__((alias("flexiblas_chain_zlaswp_")));
#else
void flexiblas_chain_zlaswp(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx){flexiblas_chain_zlaswp_((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);}
#endif



