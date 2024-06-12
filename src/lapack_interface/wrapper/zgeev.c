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


static TLS_STORE uint8_t hook_pos_zgeev = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgeev,ZGEEV)(char* jobvl, char* jobvr, blasint* n, double complex* a, blasint* lda, double complex* w, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr)
#else
void FC_GLOBAL(zgeev,ZGEEV)(char* jobvl, char* jobvr, blasint* n, double complex* a, blasint* lda, double complex* w, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr)
#endif
{
    void (*fn) (void* jobvl, void* jobvr, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);
    void (*fn_hook) (void* jobvl, void* jobvr, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zgeev.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zgeev.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr);
        return;
    } else {
        hook_pos_zgeev = 0;
        fn_hook((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void zgeev_(char* jobvl, char* jobvr, blasint* n, double complex* a, blasint* lda, double complex* w, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr) __attribute__((alias(MTS(FC_GLOBAL(zgeev,ZGEEV)))));
#else
#ifndef __APPLE__
void zgeev(char* jobvl, char* jobvr, blasint* n, double complex* a, blasint* lda, double complex* w, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr) __attribute__((alias(MTS(FC_GLOBAL(zgeev,ZGEEV)))));
#else
void zgeev(char* jobvl, char* jobvr, blasint* n, double complex* a, blasint* lda, double complex* w, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr){ FC_GLOBAL(zgeev,ZGEEV)((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgeev_(void* jobvl, void* jobvr, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr)
{
    void (*fn) (void* jobvl, void* jobvr, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    *(void **) & fn = current_backend->lapack.zgeev.f77_blas_function;

    fn((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zgeev(void* jobvl, void* jobvr, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr) __attribute__((alias("flexiblas_real_zgeev_")));
#else
void flexiblas_real_zgeev(void* jobvl, void* jobvr, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr){flexiblas_real_zgeev_((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgeev_(void* jobvl, void* jobvr, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr)
{
    void (*fn) (void* jobvl, void* jobvr, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);
    void (*fn_hook) (void* jobvl, void* jobvr, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    *(void **) &fn      = current_backend->lapack.zgeev.f77_blas_function;

    hook_pos_zgeev ++;
    if( hook_pos_zgeev < __flexiblas_hooks->zgeev.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgeev.f77_hook_function[hook_pos_zgeev];
        fn_hook((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr);
    } else {
        hook_pos_zgeev = 0;
        fn((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zgeev(void* jobvl, void* jobvr, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr) __attribute__((alias("flexiblas_chain_zgeev_")));
#else
void flexiblas_chain_zgeev(void* jobvl, void* jobvr, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr){flexiblas_chain_zgeev_((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr);}
#endif



