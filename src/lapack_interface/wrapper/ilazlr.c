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


static TLS_STORE uint8_t hook_pos_ilazlr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilazlr,ILAZLR)(blasint* m, blasint* n, double complex* a, blasint* lda)
#else
blasint FC_GLOBAL(ilazlr,ILAZLR)(blasint* m, blasint* n, double complex* a, blasint* lda)
#endif
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilazlr.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilazlr.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    } else {
        hook_pos_ilazlr = 0;
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilazlr,ILAZLR)(blasint* m, blasint* n, double complex* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilazlr,ILAZLR)))));
blasint FC_GLOBAL3(ilazlr,ILAZLR)(blasint* m, blasint* n, double complex* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilazlr,ILAZLR)))));
#else
blasint FC_GLOBAL2(ilazlr,ILAZLR)(blasint* m, blasint* n, double complex* a, blasint* lda){ return FC_GLOBAL(ilazlr,ILAZLR)((void*) m, (void*) n, (void*) a, (void*) lda); }
blasint FC_GLOBAL3(ilazlr,ILAZLR)(blasint* m, blasint* n, double complex* a, blasint* lda){ return FC_GLOBAL(ilazlr,ILAZLR)((void*) m, (void*) n, (void*) a, (void*) lda); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilazlr_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilazlr.f77_blas_function;

    ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilazlr(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_real_ilazlr_")));
#else
blasint flexiblas_real_ilazlr(void* m, void* n, void* a, void* lda){return flexiblas_real_ilazlr_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilazlr_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilazlr.f77_blas_function;

    hook_pos_ilazlr ++;
    if( hook_pos_ilazlr < __flexiblas_hooks->ilazlr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilazlr.f77_hook_function[hook_pos_ilazlr];
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
    } else {
        hook_pos_ilazlr = 0;
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilazlr(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_chain_ilazlr_")));
#else
blasint flexiblas_chain_ilazlr(void* m, void* n, void* a, void* lda){return flexiblas_chain_ilazlr_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif



