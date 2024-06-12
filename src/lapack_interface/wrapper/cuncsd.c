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


static TLS_STORE uint8_t hook_pos_cuncsd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cuncsd,CUNCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, float complex* x11, blasint* ldx11, float complex* x12, blasint* ldx12, float complex* x21, blasint* ldx21, float complex* x22, blasint* ldx22, float* theta, float complex* u1, blasint* ldu1, float complex* u2, blasint* ldu2, float complex* v1t, blasint* ldv1t, float complex* v2t, blasint* ldv2t, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs)
#else
void FC_GLOBAL(cuncsd,CUNCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, float complex* x11, blasint* ldx11, float complex* x12, blasint* ldx12, float complex* x21, blasint* ldx21, float complex* x22, blasint* ldx22, float* theta, float complex* u1, blasint* ldu1, float complex* u2, blasint* ldu2, float complex* v1t, blasint* ldv1t, float complex* v2t, blasint* ldv2t, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs)
#endif
{
    void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);
    void (*fn_hook) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.cuncsd.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->cuncsd.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_signs);
        return;
    } else {
        hook_pos_cuncsd = 0;
        fn_hook((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_signs);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void cuncsd_(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, float complex* x11, blasint* ldx11, float complex* x12, blasint* ldx12, float complex* x21, blasint* ldx21, float complex* x22, blasint* ldx22, float* theta, float complex* u1, blasint* ldu1, float complex* u2, blasint* ldu2, float complex* v1t, blasint* ldv1t, float complex* v2t, blasint* ldv2t, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs) __attribute__((alias(MTS(FC_GLOBAL(cuncsd,CUNCSD)))));
#else
#ifndef __APPLE__
void cuncsd(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, float complex* x11, blasint* ldx11, float complex* x12, blasint* ldx12, float complex* x21, blasint* ldx21, float complex* x22, blasint* ldx22, float* theta, float complex* u1, blasint* ldu1, float complex* u2, blasint* ldu2, float complex* v1t, blasint* ldv1t, float complex* v2t, blasint* ldv2t, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs) __attribute__((alias(MTS(FC_GLOBAL(cuncsd,CUNCSD)))));
#else
void cuncsd(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, float complex* x11, blasint* ldx11, float complex* x12, blasint* ldx12, float complex* x21, blasint* ldx21, float complex* x22, blasint* ldx22, float* theta, float complex* u1, blasint* ldu1, float complex* u2, blasint* ldu2, float complex* v1t, blasint* ldv1t, float complex* v2t, blasint* ldv2t, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs){ FC_GLOBAL(cuncsd,CUNCSD)((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu1, (flexiblas_fortran_charlen_t) len_jobu2, (flexiblas_fortran_charlen_t) len_jobv1t, (flexiblas_fortran_charlen_t) len_jobv2t, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_signs); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cuncsd_(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs)
{
    void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

    *(void **) & fn = current_backend->lapack.cuncsd.f77_blas_function;

    fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_signs);

    return;
}
#ifndef __APPLE__
void flexiblas_real_cuncsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs) __attribute__((alias("flexiblas_real_cuncsd_")));
#else
void flexiblas_real_cuncsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs){flexiblas_real_cuncsd_((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu1, (flexiblas_fortran_charlen_t) len_jobu2, (flexiblas_fortran_charlen_t) len_jobv1t, (flexiblas_fortran_charlen_t) len_jobv2t, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_signs);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cuncsd_(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs)
{
    void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);
    void (*fn_hook) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

    *(void **) &fn      = current_backend->lapack.cuncsd.f77_blas_function;

    hook_pos_cuncsd ++;
    if( hook_pos_cuncsd < __flexiblas_hooks->cuncsd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cuncsd.f77_hook_function[hook_pos_cuncsd];
        fn_hook((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_signs);
    } else {
        hook_pos_cuncsd = 0;
        fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobu1, ( flexiblas_fortran_charlen_t ) len_jobu2, ( flexiblas_fortran_charlen_t ) len_jobv1t, ( flexiblas_fortran_charlen_t ) len_jobv2t, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_signs);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_cuncsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs) __attribute__((alias("flexiblas_chain_cuncsd_")));
#else
void flexiblas_chain_cuncsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs){flexiblas_chain_cuncsd_((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobu1, (flexiblas_fortran_charlen_t) len_jobu2, (flexiblas_fortran_charlen_t) len_jobv1t, (flexiblas_fortran_charlen_t) len_jobv2t, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_signs);}
#endif



