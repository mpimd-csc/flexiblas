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


static TLS_STORE uint8_t hook_pos_sggglm = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sggglm,SGGGLM)(blasint* n, blasint* m, blasint* p, float* a, blasint* lda, float* b, blasint* ldb, float* d, float* x, float* y, float* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL(sggglm,SGGGLM)(blasint* n, blasint* m, blasint* p, float* a, blasint* lda, float* b, blasint* ldb, float* d, float* x, float* y, float* work, blasint* lwork, blasint* info)
#endif
{
    void (*fn) (void* n, void* m, void* p, void* a, void* lda, void* b, void* ldb, void* d, void* x, void* y, void* work, void* lwork, void* info);
    void (*fn_hook) (void* n, void* m, void* p, void* a, void* lda, void* b, void* ldb, void* d, void* x, void* y, void* work, void* lwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.sggglm.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->sggglm.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) m, (void*) p, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) d, (void*) x, (void*) y, (void*) work, (void*) lwork, (void*) info);
        return;
    } else {
        hook_pos_sggglm = 0;
        fn_hook((void*) n, (void*) m, (void*) p, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) d, (void*) x, (void*) y, (void*) work, (void*) lwork, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(sggglm,SGGGLM)(blasint* n, blasint* m, blasint* p, float* a, blasint* lda, float* b, blasint* ldb, float* d, float* x, float* y, float* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sggglm,SGGGLM)))));
void FC_GLOBAL3(sggglm,SGGGLM)(blasint* n, blasint* m, blasint* p, float* a, blasint* lda, float* b, blasint* ldb, float* d, float* x, float* y, float* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sggglm,SGGGLM)))));
#else
void FC_GLOBAL2(sggglm,SGGGLM)(blasint* n, blasint* m, blasint* p, float* a, blasint* lda, float* b, blasint* ldb, float* d, float* x, float* y, float* work, blasint* lwork, blasint* info){ FC_GLOBAL(sggglm,SGGGLM)((void*) n, (void*) m, (void*) p, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) d, (void*) x, (void*) y, (void*) work, (void*) lwork, (void*) info); }
void FC_GLOBAL3(sggglm,SGGGLM)(blasint* n, blasint* m, blasint* p, float* a, blasint* lda, float* b, blasint* ldb, float* d, float* x, float* y, float* work, blasint* lwork, blasint* info){ FC_GLOBAL(sggglm,SGGGLM)((void*) n, (void*) m, (void*) p, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) d, (void*) x, (void*) y, (void*) work, (void*) lwork, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sggglm_(void* n, void* m, void* p, void* a, void* lda, void* b, void* ldb, void* d, void* x, void* y, void* work, void* lwork, void* info)
{
    void (*fn) (void* n, void* m, void* p, void* a, void* lda, void* b, void* ldb, void* d, void* x, void* y, void* work, void* lwork, void* info);

    *(void **) & fn = current_backend->lapack.sggglm.f77_blas_function;

    fn((void*) n, (void*) m, (void*) p, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) d, (void*) x, (void*) y, (void*) work, (void*) lwork, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_sggglm(void* n, void* m, void* p, void* a, void* lda, void* b, void* ldb, void* d, void* x, void* y, void* work, void* lwork, void* info) __attribute__((alias("flexiblas_real_sggglm_")));
#else
void flexiblas_real_sggglm(void* n, void* m, void* p, void* a, void* lda, void* b, void* ldb, void* d, void* x, void* y, void* work, void* lwork, void* info){flexiblas_real_sggglm_((void*) n, (void*) m, (void*) p, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) d, (void*) x, (void*) y, (void*) work, (void*) lwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sggglm_(void* n, void* m, void* p, void* a, void* lda, void* b, void* ldb, void* d, void* x, void* y, void* work, void* lwork, void* info)
{
    void (*fn) (void* n, void* m, void* p, void* a, void* lda, void* b, void* ldb, void* d, void* x, void* y, void* work, void* lwork, void* info);
    void (*fn_hook) (void* n, void* m, void* p, void* a, void* lda, void* b, void* ldb, void* d, void* x, void* y, void* work, void* lwork, void* info);

    *(void **) &fn      = current_backend->lapack.sggglm.f77_blas_function;

    hook_pos_sggglm ++;
    if( hook_pos_sggglm < __flexiblas_hooks->sggglm.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sggglm.f77_hook_function[hook_pos_sggglm];
        fn_hook((void*) n, (void*) m, (void*) p, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) d, (void*) x, (void*) y, (void*) work, (void*) lwork, (void*) info);
    } else {
        hook_pos_sggglm = 0;
        fn((void*) n, (void*) m, (void*) p, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) d, (void*) x, (void*) y, (void*) work, (void*) lwork, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_sggglm(void* n, void* m, void* p, void* a, void* lda, void* b, void* ldb, void* d, void* x, void* y, void* work, void* lwork, void* info) __attribute__((alias("flexiblas_chain_sggglm_")));
#else
void flexiblas_chain_sggglm(void* n, void* m, void* p, void* a, void* lda, void* b, void* ldb, void* d, void* x, void* y, void* work, void* lwork, void* info){flexiblas_chain_sggglm_((void*) n, (void*) m, (void*) p, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) d, (void*) x, (void*) y, (void*) work, (void*) lwork, (void*) info);}
#endif



