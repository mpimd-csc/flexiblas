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


static TLS_STORE uint8_t hook_pos_sgesvdq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgesvdq,SGESVDQ)(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv)
#else
void FC_GLOBAL(sgesvdq,SGESVDQ)(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv)
#endif
{
    void (*fn) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv);
    void (*fn_hook) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.sgesvdq.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->sgesvdq.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_joba, ( flexiblas_fortran_charlen_t ) len_jobp, ( flexiblas_fortran_charlen_t ) len_jobr, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv);
        return;
    } else {
        hook_pos_sgesvdq = 0;
        fn_hook((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_joba, ( flexiblas_fortran_charlen_t ) len_jobp, ( flexiblas_fortran_charlen_t ) len_jobr, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(sgesvdq,SGESVDQ)(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv) __attribute__((alias(MTS(FC_GLOBAL(sgesvdq,SGESVDQ)))));
void FC_GLOBAL3(sgesvdq,SGESVDQ)(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv) __attribute__((alias(MTS(FC_GLOBAL(sgesvdq,SGESVDQ)))));
#else
void FC_GLOBAL2(sgesvdq,SGESVDQ)(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv){ FC_GLOBAL(sgesvdq,SGESVDQ)((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_joba, (flexiblas_fortran_charlen_t) len_jobp, (flexiblas_fortran_charlen_t) len_jobr, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobv); }
void FC_GLOBAL3(sgesvdq,SGESVDQ)(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv){ FC_GLOBAL(sgesvdq,SGESVDQ)((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_joba, (flexiblas_fortran_charlen_t) len_jobp, (flexiblas_fortran_charlen_t) len_jobr, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobv); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgesvdq_(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv)
{
    void (*fn) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv);

    *(void **) & fn = current_backend->lapack.sgesvdq.f77_blas_function;

    fn((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_joba, ( flexiblas_fortran_charlen_t ) len_jobp, ( flexiblas_fortran_charlen_t ) len_jobr, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv);

    return;
}
#ifndef __APPLE__
void flexiblas_real_sgesvdq(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv) __attribute__((alias("flexiblas_real_sgesvdq_")));
#else
void flexiblas_real_sgesvdq(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv){flexiblas_real_sgesvdq_((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_joba, (flexiblas_fortran_charlen_t) len_jobp, (flexiblas_fortran_charlen_t) len_jobr, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobv);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sgesvdq_(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv)
{
    void (*fn) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv);
    void (*fn_hook) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv);

    *(void **) &fn      = current_backend->lapack.sgesvdq.f77_blas_function;

    hook_pos_sgesvdq ++;
    if( hook_pos_sgesvdq < __flexiblas_hooks->sgesvdq.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sgesvdq.f77_hook_function[hook_pos_sgesvdq];
        fn_hook((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_joba, ( flexiblas_fortran_charlen_t ) len_jobp, ( flexiblas_fortran_charlen_t ) len_jobr, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv);
    } else {
        hook_pos_sgesvdq = 0;
        fn((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_joba, ( flexiblas_fortran_charlen_t ) len_jobp, ( flexiblas_fortran_charlen_t ) len_jobr, ( flexiblas_fortran_charlen_t ) len_jobu, ( flexiblas_fortran_charlen_t ) len_jobv);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_sgesvdq(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv) __attribute__((alias("flexiblas_chain_sgesvdq_")));
#else
void flexiblas_chain_sgesvdq(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobp, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv){flexiblas_chain_sgesvdq_((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_joba, (flexiblas_fortran_charlen_t) len_jobp, (flexiblas_fortran_charlen_t) len_jobr, (flexiblas_fortran_charlen_t) len_jobu, (flexiblas_fortran_charlen_t) len_jobv);}
#endif



