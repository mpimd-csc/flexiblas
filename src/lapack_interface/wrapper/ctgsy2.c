//  SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2024 Martin Koehler

   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with this program. If not, see <https://www.gnu.org/licenses/>.
   */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_ctgsy2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ctgsy2,CTGSY2)(char* trans, blasint* ijob, blasint* m, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* c, blasint* ldc, float complex* d, blasint* ldd, float complex* e, blasint* lde, float complex* f, blasint* ldf, float* scale, float* rdsum, float* rdscal, blasint* info, flexiblas_fortran_charlen_t len_trans)
#else
void FC_GLOBAL(ctgsy2,CTGSY2)(char* trans, blasint* ijob, blasint* m, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* c, blasint* ldc, float complex* d, blasint* ldd, float complex* e, blasint* lde, float complex* f, blasint* ldf, float* scale, float* rdsum, float* rdscal, blasint* info, flexiblas_fortran_charlen_t len_trans)
#endif
{
    void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info, flexiblas_fortran_charlen_t len_trans);
    void (*fn_hook) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info, flexiblas_fortran_charlen_t len_trans);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ctgsy2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ctgsy2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans);
        return;
    } else {
        hook_pos_ctgsy2 = 0;
        fn_hook((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void ctgsy2_(char* trans, blasint* ijob, blasint* m, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* c, blasint* ldc, float complex* d, blasint* ldd, float complex* e, blasint* lde, float complex* f, blasint* ldf, float* scale, float* rdsum, float* rdscal, blasint* info, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(ctgsy2,CTGSY2)))));
#else
#ifndef __APPLE__
void ctgsy2(char* trans, blasint* ijob, blasint* m, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* c, blasint* ldc, float complex* d, blasint* ldd, float complex* e, blasint* lde, float complex* f, blasint* ldf, float* scale, float* rdsum, float* rdscal, blasint* info, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(ctgsy2,CTGSY2)))));
#else
void ctgsy2(char* trans, blasint* ijob, blasint* m, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* c, blasint* ldc, float complex* d, blasint* ldd, float complex* e, blasint* lde, float complex* f, blasint* ldf, float* scale, float* rdsum, float* rdscal, blasint* info, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(ctgsy2,CTGSY2)((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info, (flexiblas_fortran_charlen_t) len_trans); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ctgsy2_(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info, flexiblas_fortran_charlen_t len_trans)
{
    void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info, flexiblas_fortran_charlen_t len_trans);

    *(void **) & fn = current_backend->lapack.ctgsy2.f77_blas_function;

    fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans);

    return;
}
#ifndef __APPLE__
void flexiblas_real_ctgsy2(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_ctgsy2_")));
#else
void flexiblas_real_ctgsy2(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info, flexiblas_fortran_charlen_t len_trans){flexiblas_real_ctgsy2_((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ctgsy2_(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info, flexiblas_fortran_charlen_t len_trans)
{
    void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info, flexiblas_fortran_charlen_t len_trans);
    void (*fn_hook) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info, flexiblas_fortran_charlen_t len_trans);

    *(void **) &fn      = current_backend->lapack.ctgsy2.f77_blas_function;

    hook_pos_ctgsy2 ++;
    if( hook_pos_ctgsy2 < __flexiblas_hooks->ctgsy2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ctgsy2.f77_hook_function[hook_pos_ctgsy2];
        fn_hook((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans);
    } else {
        hook_pos_ctgsy2 = 0;
        fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_ctgsy2(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_ctgsy2_")));
#else
void flexiblas_chain_ctgsy2(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info, flexiblas_fortran_charlen_t len_trans){flexiblas_chain_ctgsy2_((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info, (flexiblas_fortran_charlen_t) len_trans);}
#endif



