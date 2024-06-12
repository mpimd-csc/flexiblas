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


static TLS_STORE uint8_t hook_pos_spteqr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(spteqr,SPTEQR)(char* compz, blasint* n, float* d, float* e, float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_compz)
#else
void FC_GLOBAL(spteqr,SPTEQR)(char* compz, blasint* n, float* d, float* e, float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_compz)
#endif
{
    void (*fn) (void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz);
    void (*fn_hook) (void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.spteqr.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->spteqr.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compz);
        return;
    } else {
        hook_pos_spteqr = 0;
        fn_hook((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compz);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void spteqr_(char* compz, blasint* n, float* d, float* e, float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_compz) __attribute__((alias(MTS(FC_GLOBAL(spteqr,SPTEQR)))));
#else
#ifndef __APPLE__
void spteqr(char* compz, blasint* n, float* d, float* e, float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_compz) __attribute__((alias(MTS(FC_GLOBAL(spteqr,SPTEQR)))));
#else
void spteqr(char* compz, blasint* n, float* d, float* e, float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_compz){ FC_GLOBAL(spteqr,SPTEQR)((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_compz); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_spteqr_(void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz)
{
    void (*fn) (void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz);

    *(void **) & fn = current_backend->lapack.spteqr.f77_blas_function;

    fn((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compz);

    return;
}
#ifndef __APPLE__
void flexiblas_real_spteqr(void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz) __attribute__((alias("flexiblas_real_spteqr_")));
#else
void flexiblas_real_spteqr(void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz){flexiblas_real_spteqr_((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_compz);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_spteqr_(void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz)
{
    void (*fn) (void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz);
    void (*fn_hook) (void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz);

    *(void **) &fn      = current_backend->lapack.spteqr.f77_blas_function;

    hook_pos_spteqr ++;
    if( hook_pos_spteqr < __flexiblas_hooks->spteqr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->spteqr.f77_hook_function[hook_pos_spteqr];
        fn_hook((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compz);
    } else {
        hook_pos_spteqr = 0;
        fn((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compz);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_spteqr(void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz) __attribute__((alias("flexiblas_chain_spteqr_")));
#else
void flexiblas_chain_spteqr(void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz){flexiblas_chain_spteqr_((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_compz);}
#endif



