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


static TLS_STORE uint8_t hook_pos_clarcm = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clarcm,CLARCM)(blasint* m, blasint* n, float* a, blasint* lda, float complex* b, blasint* ldb, float complex* c, blasint* ldc, float* rwork)
#else
void FC_GLOBAL(clarcm,CLARCM)(blasint* m, blasint* n, float* a, blasint* lda, float complex* b, blasint* ldb, float complex* c, blasint* ldc, float* rwork)
#endif
{
    void (*fn) (void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork);
    void (*fn_hook) (void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.clarcm.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->clarcm.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);
        return;
    } else {
        hook_pos_clarcm = 0;
        fn_hook((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(clarcm,CLARCM)(blasint* m, blasint* n, float* a, blasint* lda, float complex* b, blasint* ldb, float complex* c, blasint* ldc, float* rwork) __attribute__((alias(MTS(FC_GLOBAL(clarcm,CLARCM)))));
void FC_GLOBAL3(clarcm,CLARCM)(blasint* m, blasint* n, float* a, blasint* lda, float complex* b, blasint* ldb, float complex* c, blasint* ldc, float* rwork) __attribute__((alias(MTS(FC_GLOBAL(clarcm,CLARCM)))));
#else
void FC_GLOBAL2(clarcm,CLARCM)(blasint* m, blasint* n, float* a, blasint* lda, float complex* b, blasint* ldb, float complex* c, blasint* ldc, float* rwork){ FC_GLOBAL(clarcm,CLARCM)((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork); }
void FC_GLOBAL3(clarcm,CLARCM)(blasint* m, blasint* n, float* a, blasint* lda, float complex* b, blasint* ldb, float complex* c, blasint* ldc, float* rwork){ FC_GLOBAL(clarcm,CLARCM)((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clarcm_(void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork)
{
    void (*fn) (void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork);

    *(void **) & fn = current_backend->lapack.clarcm.f77_blas_function;

    fn((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);

    return;
}
#ifndef __APPLE__
void flexiblas_real_clarcm(void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork) __attribute__((alias("flexiblas_real_clarcm_")));
#else
void flexiblas_real_clarcm(void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork){flexiblas_real_clarcm_((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clarcm_(void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork)
{
    void (*fn) (void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork);
    void (*fn_hook) (void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork);

    *(void **) &fn      = current_backend->lapack.clarcm.f77_blas_function;

    hook_pos_clarcm ++;
    if( hook_pos_clarcm < __flexiblas_hooks->clarcm.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clarcm.f77_hook_function[hook_pos_clarcm];
        fn_hook((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);
    } else {
        hook_pos_clarcm = 0;
        fn((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_clarcm(void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork) __attribute__((alias("flexiblas_chain_clarcm_")));
#else
void flexiblas_chain_clarcm(void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork){flexiblas_chain_clarcm_((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);}
#endif



