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


static TLS_STORE uint8_t hook_pos_clahr2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clahr2,CLAHR2)(blasint* n, blasint* k, blasint* nb, float complex* a, blasint* lda, float complex* tau, float complex* t, blasint* ldt, float complex* y, blasint* ldy)
#else
void FC_GLOBAL(clahr2,CLAHR2)(blasint* n, blasint* k, blasint* nb, float complex* a, blasint* lda, float complex* tau, float complex* t, blasint* ldt, float complex* y, blasint* ldy)
#endif
{
    void (*fn) (void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy);
    void (*fn_hook) (void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.clahr2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->clahr2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy);
        return;
    } else {
        hook_pos_clahr2 = 0;
        fn_hook((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(clahr2,CLAHR2)(blasint* n, blasint* k, blasint* nb, float complex* a, blasint* lda, float complex* tau, float complex* t, blasint* ldt, float complex* y, blasint* ldy) __attribute__((alias(MTS(FC_GLOBAL(clahr2,CLAHR2)))));
void FC_GLOBAL3(clahr2,CLAHR2)(blasint* n, blasint* k, blasint* nb, float complex* a, blasint* lda, float complex* tau, float complex* t, blasint* ldt, float complex* y, blasint* ldy) __attribute__((alias(MTS(FC_GLOBAL(clahr2,CLAHR2)))));
#else
void FC_GLOBAL2(clahr2,CLAHR2)(blasint* n, blasint* k, blasint* nb, float complex* a, blasint* lda, float complex* tau, float complex* t, blasint* ldt, float complex* y, blasint* ldy){ FC_GLOBAL(clahr2,CLAHR2)((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy); }
void FC_GLOBAL3(clahr2,CLAHR2)(blasint* n, blasint* k, blasint* nb, float complex* a, blasint* lda, float complex* tau, float complex* t, blasint* ldt, float complex* y, blasint* ldy){ FC_GLOBAL(clahr2,CLAHR2)((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clahr2_(void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy)
{
    void (*fn) (void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy);

    *(void **) & fn = current_backend->lapack.clahr2.f77_blas_function;

    fn((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy);

    return;
}
#ifndef __APPLE__
void flexiblas_real_clahr2(void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy) __attribute__((alias("flexiblas_real_clahr2_")));
#else
void flexiblas_real_clahr2(void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy){flexiblas_real_clahr2_((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clahr2_(void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy)
{
    void (*fn) (void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy);
    void (*fn_hook) (void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy);

    *(void **) &fn      = current_backend->lapack.clahr2.f77_blas_function;

    hook_pos_clahr2 ++;
    if( hook_pos_clahr2 < __flexiblas_hooks->clahr2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clahr2.f77_hook_function[hook_pos_clahr2];
        fn_hook((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy);
    } else {
        hook_pos_clahr2 = 0;
        fn((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_clahr2(void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy) __attribute__((alias("flexiblas_chain_clahr2_")));
#else
void flexiblas_chain_clahr2(void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy){flexiblas_chain_clahr2_((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy);}
#endif



