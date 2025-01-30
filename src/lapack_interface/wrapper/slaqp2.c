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


static TLS_STORE uint8_t hook_pos_slaqp2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaqp2,SLAQP2)(blasint* m, blasint* n, blasint* offset, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* work)
#else
void FC_GLOBAL(slaqp2,SLAQP2)(blasint* m, blasint* n, blasint* offset, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* work)
#endif
{
    void (*fn) (void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work);
    void (*fn_hook) (void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slaqp2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slaqp2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work);
        return;
    } else {
        hook_pos_slaqp2 = 0;
        fn_hook((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slaqp2,SLAQP2)(blasint* m, blasint* n, blasint* offset, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* work) __attribute__((alias(MTS(FC_GLOBAL(slaqp2,SLAQP2)))));
void FC_GLOBAL3(slaqp2,SLAQP2)(blasint* m, blasint* n, blasint* offset, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* work) __attribute__((alias(MTS(FC_GLOBAL(slaqp2,SLAQP2)))));
#else
void FC_GLOBAL2(slaqp2,SLAQP2)(blasint* m, blasint* n, blasint* offset, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* work){ FC_GLOBAL(slaqp2,SLAQP2)((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work); }
void FC_GLOBAL3(slaqp2,SLAQP2)(blasint* m, blasint* n, blasint* offset, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* work){ FC_GLOBAL(slaqp2,SLAQP2)((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaqp2_(void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work)
{
    void (*fn) (void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work);

    *(void **) & fn = current_backend->lapack.slaqp2.f77_blas_function;

    fn((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slaqp2(void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work) __attribute__((alias("flexiblas_real_slaqp2_")));
#else
void flexiblas_real_slaqp2(void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work){flexiblas_real_slaqp2_((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slaqp2_(void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work)
{
    void (*fn) (void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work);
    void (*fn_hook) (void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work);

    *(void **) &fn      = current_backend->lapack.slaqp2.f77_blas_function;

    hook_pos_slaqp2 ++;
    if( hook_pos_slaqp2 < __flexiblas_hooks->slaqp2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slaqp2.f77_hook_function[hook_pos_slaqp2];
        fn_hook((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work);
    } else {
        hook_pos_slaqp2 = 0;
        fn((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slaqp2(void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work) __attribute__((alias("flexiblas_chain_slaqp2_")));
#else
void flexiblas_chain_slaqp2(void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work){flexiblas_chain_slaqp2_((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work);}
#endif



