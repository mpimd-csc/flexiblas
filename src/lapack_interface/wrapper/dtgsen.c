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


static TLS_STORE uint8_t hook_pos_dtgsen = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dtgsen,DTGSEN)(blasint* ijob, blaslogical* wantq, blaslogical* wantz, blaslogical* select, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info)
#else
void FC_GLOBAL(dtgsen,DTGSEN)(blasint* ijob, blaslogical* wantq, blaslogical* wantz, blaslogical* select, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info)
#endif
{
    void (*fn) (void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info);
    void (*fn_hook) (void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dtgsen.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dtgsen.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);
        return;
    } else {
        hook_pos_dtgsen = 0;
        fn_hook((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dtgsen,DTGSEN)(blasint* ijob, blaslogical* wantq, blaslogical* wantz, blaslogical* select, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dtgsen,DTGSEN)))));
void FC_GLOBAL3(dtgsen,DTGSEN)(blasint* ijob, blaslogical* wantq, blaslogical* wantz, blaslogical* select, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dtgsen,DTGSEN)))));
#else
void FC_GLOBAL2(dtgsen,DTGSEN)(blasint* ijob, blaslogical* wantq, blaslogical* wantz, blaslogical* select, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info){ FC_GLOBAL(dtgsen,DTGSEN)((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); }
void FC_GLOBAL3(dtgsen,DTGSEN)(blasint* ijob, blaslogical* wantq, blaslogical* wantz, blaslogical* select, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info){ FC_GLOBAL(dtgsen,DTGSEN)((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dtgsen_(void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info)
{
    void (*fn) (void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info);

    *(void **) & fn = current_backend->lapack.dtgsen.f77_blas_function;

    fn((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dtgsen(void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info) __attribute__((alias("flexiblas_real_dtgsen_")));
#else
void flexiblas_real_dtgsen(void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info){flexiblas_real_dtgsen_((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dtgsen_(void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info)
{
    void (*fn) (void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info);
    void (*fn_hook) (void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info);

    *(void **) &fn      = current_backend->lapack.dtgsen.f77_blas_function;

    hook_pos_dtgsen ++;
    if( hook_pos_dtgsen < __flexiblas_hooks->dtgsen.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dtgsen.f77_hook_function[hook_pos_dtgsen];
        fn_hook((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);
    } else {
        hook_pos_dtgsen = 0;
        fn((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dtgsen(void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info) __attribute__((alias("flexiblas_chain_dtgsen_")));
#else
void flexiblas_chain_dtgsen(void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info){flexiblas_chain_dtgsen_((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);}
#endif



