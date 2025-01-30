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


static TLS_STORE uint8_t hook_pos_sgeev = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgeev,SGEEV)(char* jobvl, char* jobvr, blasint* n, float* a, blasint* lda, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr)
#else
void FC_GLOBAL(sgeev,SGEEV)(char* jobvl, char* jobvr, blasint* n, float* a, blasint* lda, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr)
#endif
{
    void (*fn) (void* jobvl, void* jobvr, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);
    void (*fn_hook) (void* jobvl, void* jobvr, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.sgeev.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->sgeev.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr);
        return;
    } else {
        hook_pos_sgeev = 0;
        fn_hook((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(sgeev,SGEEV)(char* jobvl, char* jobvr, blasint* n, float* a, blasint* lda, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr) __attribute__((alias(MTS(FC_GLOBAL(sgeev,SGEEV)))));
void FC_GLOBAL3(sgeev,SGEEV)(char* jobvl, char* jobvr, blasint* n, float* a, blasint* lda, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr) __attribute__((alias(MTS(FC_GLOBAL(sgeev,SGEEV)))));
#else
void FC_GLOBAL2(sgeev,SGEEV)(char* jobvl, char* jobvr, blasint* n, float* a, blasint* lda, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr){ FC_GLOBAL(sgeev,SGEEV)((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr); }
void FC_GLOBAL3(sgeev,SGEEV)(char* jobvl, char* jobvr, blasint* n, float* a, blasint* lda, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr){ FC_GLOBAL(sgeev,SGEEV)((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgeev_(void* jobvl, void* jobvr, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr)
{
    void (*fn) (void* jobvl, void* jobvr, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    *(void **) & fn = current_backend->lapack.sgeev.f77_blas_function;

    fn((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr);

    return;
}
#ifndef __APPLE__
void flexiblas_real_sgeev(void* jobvl, void* jobvr, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr) __attribute__((alias("flexiblas_real_sgeev_")));
#else
void flexiblas_real_sgeev(void* jobvl, void* jobvr, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr){flexiblas_real_sgeev_((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sgeev_(void* jobvl, void* jobvr, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr)
{
    void (*fn) (void* jobvl, void* jobvr, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);
    void (*fn_hook) (void* jobvl, void* jobvr, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    *(void **) &fn      = current_backend->lapack.sgeev.f77_blas_function;

    hook_pos_sgeev ++;
    if( hook_pos_sgeev < __flexiblas_hooks->sgeev.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sgeev.f77_hook_function[hook_pos_sgeev];
        fn_hook((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr);
    } else {
        hook_pos_sgeev = 0;
        fn((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_sgeev(void* jobvl, void* jobvr, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr) __attribute__((alias("flexiblas_chain_sgeev_")));
#else
void flexiblas_chain_sgeev(void* jobvl, void* jobvr, void* n, void* a, void* lda, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr){flexiblas_chain_sgeev_((void*) jobvl, (void*) jobvr, (void*) n, (void*) a, (void*) lda, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr);}
#endif



