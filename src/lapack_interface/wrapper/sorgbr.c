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


static TLS_STORE uint8_t hook_pos_sorgbr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sorgbr,SORGBR)(char* vect, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect)
#else
void FC_GLOBAL(sorgbr,SORGBR)(char* vect, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect)
#endif
{
    void (*fn) (void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect);
    void (*fn_hook) (void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.sorgbr.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->sorgbr.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect);
        return;
    } else {
        hook_pos_sorgbr = 0;
        fn_hook((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(sorgbr,SORGBR)(char* vect, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias(MTS(FC_GLOBAL(sorgbr,SORGBR)))));
void FC_GLOBAL3(sorgbr,SORGBR)(char* vect, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias(MTS(FC_GLOBAL(sorgbr,SORGBR)))));
#else
void FC_GLOBAL2(sorgbr,SORGBR)(char* vect, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect){ FC_GLOBAL(sorgbr,SORGBR)((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_vect); }
void FC_GLOBAL3(sorgbr,SORGBR)(char* vect, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect){ FC_GLOBAL(sorgbr,SORGBR)((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_vect); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sorgbr_(void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect)
{
    void (*fn) (void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect);

    *(void **) & fn = current_backend->lapack.sorgbr.f77_blas_function;

    fn((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect);

    return;
}
#ifndef __APPLE__
void flexiblas_real_sorgbr(void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias("flexiblas_real_sorgbr_")));
#else
void flexiblas_real_sorgbr(void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect){flexiblas_real_sorgbr_((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_vect);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sorgbr_(void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect)
{
    void (*fn) (void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect);
    void (*fn_hook) (void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect);

    *(void **) &fn      = current_backend->lapack.sorgbr.f77_blas_function;

    hook_pos_sorgbr ++;
    if( hook_pos_sorgbr < __flexiblas_hooks->sorgbr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sorgbr.f77_hook_function[hook_pos_sorgbr];
        fn_hook((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect);
    } else {
        hook_pos_sorgbr = 0;
        fn((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_sorgbr(void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias("flexiblas_chain_sorgbr_")));
#else
void flexiblas_chain_sorgbr(void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect){flexiblas_chain_sorgbr_((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_vect);}
#endif



