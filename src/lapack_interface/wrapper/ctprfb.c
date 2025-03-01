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


static TLS_STORE uint8_t hook_pos_ctprfb = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ctprfb,CTPRFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, float complex* v, blasint* ldv, float complex* t, blasint* ldt, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev)
#else
void FC_GLOBAL(ctprfb,CTPRFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, float complex* v, blasint* ldv, float complex* t, blasint* ldt, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev)
#endif
{
    void (*fn) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);
    void (*fn_hook) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ctprfb.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ctprfb.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_direct, ( flexiblas_fortran_charlen_t ) len_storev);
        return;
    } else {
        hook_pos_ctprfb = 0;
        fn_hook((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_direct, ( flexiblas_fortran_charlen_t ) len_storev);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(ctprfb,CTPRFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, float complex* v, blasint* ldv, float complex* t, blasint* ldt, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev) __attribute__((alias(MTS(FC_GLOBAL(ctprfb,CTPRFB)))));
void FC_GLOBAL3(ctprfb,CTPRFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, float complex* v, blasint* ldv, float complex* t, blasint* ldt, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev) __attribute__((alias(MTS(FC_GLOBAL(ctprfb,CTPRFB)))));
#else
void FC_GLOBAL2(ctprfb,CTPRFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, float complex* v, blasint* ldv, float complex* t, blasint* ldt, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev){ FC_GLOBAL(ctprfb,CTPRFB)((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_direct, (flexiblas_fortran_charlen_t) len_storev); }
void FC_GLOBAL3(ctprfb,CTPRFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, float complex* v, blasint* ldv, float complex* t, blasint* ldt, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev){ FC_GLOBAL(ctprfb,CTPRFB)((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_direct, (flexiblas_fortran_charlen_t) len_storev); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ctprfb_(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev)
{
    void (*fn) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    *(void **) & fn = current_backend->lapack.ctprfb.f77_blas_function;

    fn((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_direct, ( flexiblas_fortran_charlen_t ) len_storev);

    return;
}
#ifndef __APPLE__
void flexiblas_real_ctprfb(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev) __attribute__((alias("flexiblas_real_ctprfb_")));
#else
void flexiblas_real_ctprfb(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev){flexiblas_real_ctprfb_((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_direct, (flexiblas_fortran_charlen_t) len_storev);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ctprfb_(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev)
{
    void (*fn) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);
    void (*fn_hook) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    *(void **) &fn      = current_backend->lapack.ctprfb.f77_blas_function;

    hook_pos_ctprfb ++;
    if( hook_pos_ctprfb < __flexiblas_hooks->ctprfb.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ctprfb.f77_hook_function[hook_pos_ctprfb];
        fn_hook((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_direct, ( flexiblas_fortran_charlen_t ) len_storev);
    } else {
        hook_pos_ctprfb = 0;
        fn((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_direct, ( flexiblas_fortran_charlen_t ) len_storev);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_ctprfb(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev) __attribute__((alias("flexiblas_chain_ctprfb_")));
#else
void flexiblas_chain_ctprfb(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev){flexiblas_chain_ctprfb_((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_direct, (flexiblas_fortran_charlen_t) len_storev);}
#endif



