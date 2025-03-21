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


static TLS_STORE uint8_t hook_pos_ilaslc = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda)
#else
blasint FC_GLOBAL(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda)
#endif
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilaslc.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilaslc.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    } else {
        hook_pos_ilaslc = 0;
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilaslc,ILASLC)))));
blasint FC_GLOBAL3(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilaslc,ILASLC)))));
#else
blasint FC_GLOBAL2(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda){ return FC_GLOBAL(ilaslc,ILASLC)((void*) m, (void*) n, (void*) a, (void*) lda); }
blasint FC_GLOBAL3(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda){ return FC_GLOBAL(ilaslc,ILASLC)((void*) m, (void*) n, (void*) a, (void*) lda); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilaslc_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilaslc.f77_blas_function;

    ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilaslc(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_real_ilaslc_")));
#else
blasint flexiblas_real_ilaslc(void* m, void* n, void* a, void* lda){return flexiblas_real_ilaslc_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilaslc_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilaslc.f77_blas_function;

    hook_pos_ilaslc ++;
    if( hook_pos_ilaslc < __flexiblas_hooks->ilaslc.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilaslc.f77_hook_function[hook_pos_ilaslc];
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
    } else {
        hook_pos_ilaslc = 0;
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilaslc(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_chain_ilaslc_")));
#else
blasint flexiblas_chain_ilaslc(void* m, void* n, void* a, void* lda){return flexiblas_chain_ilaslc_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif



