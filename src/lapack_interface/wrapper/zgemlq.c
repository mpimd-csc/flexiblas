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


static TLS_STORE uint8_t hook_pos_zgemlq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgemlq,ZGEMLQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double complex* a, blasint* lda, double complex* t, blasint* tsize, double complex* c, blasint* ldc, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
#else
void FC_GLOBAL(zgemlq,ZGEMLQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double complex* a, blasint* lda, double complex* t, blasint* tsize, double complex* c, blasint* ldc, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
#endif
{
    void (*fn) (void* side, void* trans, void* m, void* n, void* k, void* a, void* lda, void* t, void* tsize, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);
    void (*fn_hook) (void* side, void* trans, void* m, void* n, void* k, void* a, void* lda, void* t, void* tsize, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zgemlq.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zgemlq.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) t, (void*) tsize, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans);
        return;
    } else {
        hook_pos_zgemlq = 0;
        fn_hook((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) t, (void*) tsize, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zgemlq,ZGEMLQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double complex* a, blasint* lda, double complex* t, blasint* tsize, double complex* c, blasint* ldc, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(zgemlq,ZGEMLQ)))));
void FC_GLOBAL3(zgemlq,ZGEMLQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double complex* a, blasint* lda, double complex* t, blasint* tsize, double complex* c, blasint* ldc, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(zgemlq,ZGEMLQ)))));
#else
void FC_GLOBAL2(zgemlq,ZGEMLQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double complex* a, blasint* lda, double complex* t, blasint* tsize, double complex* c, blasint* ldc, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(zgemlq,ZGEMLQ)((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) t, (void*) tsize, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans); }
void FC_GLOBAL3(zgemlq,ZGEMLQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double complex* a, blasint* lda, double complex* t, blasint* tsize, double complex* c, blasint* ldc, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(zgemlq,ZGEMLQ)((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) t, (void*) tsize, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgemlq_(void* side, void* trans, void* m, void* n, void* k, void* a, void* lda, void* t, void* tsize, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
{
    void (*fn) (void* side, void* trans, void* m, void* n, void* k, void* a, void* lda, void* t, void* tsize, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    *(void **) & fn = current_backend->lapack.zgemlq.f77_blas_function;

    fn((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) t, (void*) tsize, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zgemlq(void* side, void* trans, void* m, void* n, void* k, void* a, void* lda, void* t, void* tsize, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_zgemlq_")));
#else
void flexiblas_real_zgemlq(void* side, void* trans, void* m, void* n, void* k, void* a, void* lda, void* t, void* tsize, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){flexiblas_real_zgemlq_((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) t, (void*) tsize, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgemlq_(void* side, void* trans, void* m, void* n, void* k, void* a, void* lda, void* t, void* tsize, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
{
    void (*fn) (void* side, void* trans, void* m, void* n, void* k, void* a, void* lda, void* t, void* tsize, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);
    void (*fn_hook) (void* side, void* trans, void* m, void* n, void* k, void* a, void* lda, void* t, void* tsize, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    *(void **) &fn      = current_backend->lapack.zgemlq.f77_blas_function;

    hook_pos_zgemlq ++;
    if( hook_pos_zgemlq < __flexiblas_hooks->zgemlq.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgemlq.f77_hook_function[hook_pos_zgemlq];
        fn_hook((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) t, (void*) tsize, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans);
    } else {
        hook_pos_zgemlq = 0;
        fn((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) t, (void*) tsize, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zgemlq(void* side, void* trans, void* m, void* n, void* k, void* a, void* lda, void* t, void* tsize, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_zgemlq_")));
#else
void flexiblas_chain_zgemlq(void* side, void* trans, void* m, void* n, void* k, void* a, void* lda, void* t, void* tsize, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){flexiblas_chain_zgemlq_((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) t, (void*) tsize, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans);}
#endif



