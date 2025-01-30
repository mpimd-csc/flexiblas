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


static TLS_STORE uint8_t hook_pos_cgelss = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgelss,CGELSS)(blasint* m, blasint* n, blasint* nrhs, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* s, float* rcond, blasint* rank_bn, float complex* work, blasint* lwork, float* rwork, blasint* info)
#else
void FC_GLOBAL(cgelss,CGELSS)(blasint* m, blasint* n, blasint* nrhs, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* s, float* rcond, blasint* rank_bn, float complex* work, blasint* lwork, float* rwork, blasint* info)
#endif
{
    void (*fn) (void* m, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* s, void* rcond, void* rank_bn, void* work, void* lwork, void* rwork, void* info);
    void (*fn_hook) (void* m, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* s, void* rcond, void* rank_bn, void* work, void* lwork, void* rwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.cgelss.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->cgelss.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) m, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) s, (void*) rcond, (void*) rank_bn, (void*) work, (void*) lwork, (void*) rwork, (void*) info);
        return;
    } else {
        hook_pos_cgelss = 0;
        fn_hook((void*) m, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) s, (void*) rcond, (void*) rank_bn, (void*) work, (void*) lwork, (void*) rwork, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(cgelss,CGELSS)(blasint* m, blasint* n, blasint* nrhs, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* s, float* rcond, blasint* rank_bn, float complex* work, blasint* lwork, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgelss,CGELSS)))));
void FC_GLOBAL3(cgelss,CGELSS)(blasint* m, blasint* n, blasint* nrhs, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* s, float* rcond, blasint* rank_bn, float complex* work, blasint* lwork, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgelss,CGELSS)))));
#else
void FC_GLOBAL2(cgelss,CGELSS)(blasint* m, blasint* n, blasint* nrhs, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* s, float* rcond, blasint* rank_bn, float complex* work, blasint* lwork, float* rwork, blasint* info){ FC_GLOBAL(cgelss,CGELSS)((void*) m, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) s, (void*) rcond, (void*) rank_bn, (void*) work, (void*) lwork, (void*) rwork, (void*) info); }
void FC_GLOBAL3(cgelss,CGELSS)(blasint* m, blasint* n, blasint* nrhs, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* s, float* rcond, blasint* rank_bn, float complex* work, blasint* lwork, float* rwork, blasint* info){ FC_GLOBAL(cgelss,CGELSS)((void*) m, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) s, (void*) rcond, (void*) rank_bn, (void*) work, (void*) lwork, (void*) rwork, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgelss_(void* m, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* s, void* rcond, void* rank_bn, void* work, void* lwork, void* rwork, void* info)
{
    void (*fn) (void* m, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* s, void* rcond, void* rank_bn, void* work, void* lwork, void* rwork, void* info);

    *(void **) & fn = current_backend->lapack.cgelss.f77_blas_function;

    fn((void*) m, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) s, (void*) rcond, (void*) rank_bn, (void*) work, (void*) lwork, (void*) rwork, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_cgelss(void* m, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* s, void* rcond, void* rank_bn, void* work, void* lwork, void* rwork, void* info) __attribute__((alias("flexiblas_real_cgelss_")));
#else
void flexiblas_real_cgelss(void* m, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* s, void* rcond, void* rank_bn, void* work, void* lwork, void* rwork, void* info){flexiblas_real_cgelss_((void*) m, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) s, (void*) rcond, (void*) rank_bn, (void*) work, (void*) lwork, (void*) rwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgelss_(void* m, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* s, void* rcond, void* rank_bn, void* work, void* lwork, void* rwork, void* info)
{
    void (*fn) (void* m, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* s, void* rcond, void* rank_bn, void* work, void* lwork, void* rwork, void* info);
    void (*fn_hook) (void* m, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* s, void* rcond, void* rank_bn, void* work, void* lwork, void* rwork, void* info);

    *(void **) &fn      = current_backend->lapack.cgelss.f77_blas_function;

    hook_pos_cgelss ++;
    if( hook_pos_cgelss < __flexiblas_hooks->cgelss.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgelss.f77_hook_function[hook_pos_cgelss];
        fn_hook((void*) m, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) s, (void*) rcond, (void*) rank_bn, (void*) work, (void*) lwork, (void*) rwork, (void*) info);
    } else {
        hook_pos_cgelss = 0;
        fn((void*) m, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) s, (void*) rcond, (void*) rank_bn, (void*) work, (void*) lwork, (void*) rwork, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_cgelss(void* m, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* s, void* rcond, void* rank_bn, void* work, void* lwork, void* rwork, void* info) __attribute__((alias("flexiblas_chain_cgelss_")));
#else
void flexiblas_chain_cgelss(void* m, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* s, void* rcond, void* rank_bn, void* work, void* lwork, void* rwork, void* info){flexiblas_chain_cgelss_((void*) m, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) s, (void*) rcond, (void*) rank_bn, (void*) work, (void*) lwork, (void*) rwork, (void*) info);}
#endif



