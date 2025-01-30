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


static TLS_STORE uint8_t hook_pos_zlacrm = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlacrm,ZLACRM)(blasint* m, blasint* n, double complex* a, blasint* lda, double* b, blasint* ldb, double complex* c, blasint* ldc, double* rwork)
#else
void FC_GLOBAL(zlacrm,ZLACRM)(blasint* m, blasint* n, double complex* a, blasint* lda, double* b, blasint* ldb, double complex* c, blasint* ldc, double* rwork)
#endif
{
    void (*fn) (void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork);
    void (*fn_hook) (void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlacrm.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlacrm.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);
        return;
    } else {
        hook_pos_zlacrm = 0;
        fn_hook((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zlacrm,ZLACRM)(blasint* m, blasint* n, double complex* a, blasint* lda, double* b, blasint* ldb, double complex* c, blasint* ldc, double* rwork) __attribute__((alias(MTS(FC_GLOBAL(zlacrm,ZLACRM)))));
void FC_GLOBAL3(zlacrm,ZLACRM)(blasint* m, blasint* n, double complex* a, blasint* lda, double* b, blasint* ldb, double complex* c, blasint* ldc, double* rwork) __attribute__((alias(MTS(FC_GLOBAL(zlacrm,ZLACRM)))));
#else
void FC_GLOBAL2(zlacrm,ZLACRM)(blasint* m, blasint* n, double complex* a, blasint* lda, double* b, blasint* ldb, double complex* c, blasint* ldc, double* rwork){ FC_GLOBAL(zlacrm,ZLACRM)((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork); }
void FC_GLOBAL3(zlacrm,ZLACRM)(blasint* m, blasint* n, double complex* a, blasint* lda, double* b, blasint* ldb, double complex* c, blasint* ldc, double* rwork){ FC_GLOBAL(zlacrm,ZLACRM)((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlacrm_(void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork)
{
    void (*fn) (void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork);

    *(void **) & fn = current_backend->lapack.zlacrm.f77_blas_function;

    fn((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlacrm(void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork) __attribute__((alias("flexiblas_real_zlacrm_")));
#else
void flexiblas_real_zlacrm(void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork){flexiblas_real_zlacrm_((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlacrm_(void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork)
{
    void (*fn) (void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork);
    void (*fn_hook) (void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork);

    *(void **) &fn      = current_backend->lapack.zlacrm.f77_blas_function;

    hook_pos_zlacrm ++;
    if( hook_pos_zlacrm < __flexiblas_hooks->zlacrm.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlacrm.f77_hook_function[hook_pos_zlacrm];
        fn_hook((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);
    } else {
        hook_pos_zlacrm = 0;
        fn((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlacrm(void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork) __attribute__((alias("flexiblas_chain_zlacrm_")));
#else
void flexiblas_chain_zlacrm(void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* rwork){flexiblas_chain_zlacrm_((void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) rwork);}
#endif



