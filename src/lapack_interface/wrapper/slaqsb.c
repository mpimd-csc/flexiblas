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


static TLS_STORE uint8_t hook_pos_slaqsb = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaqsb,SLAQSB)(char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed)
#else
void FC_GLOBAL(slaqsb,SLAQSB)(char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed)
#endif
{
    void (*fn) (void* uplo, void* n, void* kd, void* ab, void* ldab, void* s, void* scond, void* amax, void* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);
    void (*fn_hook) (void* uplo, void* n, void* kd, void* ab, void* ldab, void* s, void* scond, void* amax, void* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slaqsb.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slaqsb.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) s, (void*) scond, (void*) amax, (void*) equed, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_equed);
        return;
    } else {
        hook_pos_slaqsb = 0;
        fn_hook((void*) uplo, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) s, (void*) scond, (void*) amax, (void*) equed, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_equed);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slaqsb,SLAQSB)(char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed) __attribute__((alias(MTS(FC_GLOBAL(slaqsb,SLAQSB)))));
void FC_GLOBAL3(slaqsb,SLAQSB)(char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed) __attribute__((alias(MTS(FC_GLOBAL(slaqsb,SLAQSB)))));
#else
void FC_GLOBAL2(slaqsb,SLAQSB)(char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed){ FC_GLOBAL(slaqsb,SLAQSB)((void*) uplo, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) s, (void*) scond, (void*) amax, (void*) equed, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_equed); }
void FC_GLOBAL3(slaqsb,SLAQSB)(char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed){ FC_GLOBAL(slaqsb,SLAQSB)((void*) uplo, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) s, (void*) scond, (void*) amax, (void*) equed, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_equed); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaqsb_(void* uplo, void* n, void* kd, void* ab, void* ldab, void* s, void* scond, void* amax, void* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed)
{
    void (*fn) (void* uplo, void* n, void* kd, void* ab, void* ldab, void* s, void* scond, void* amax, void* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    *(void **) & fn = current_backend->lapack.slaqsb.f77_blas_function;

    fn((void*) uplo, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) s, (void*) scond, (void*) amax, (void*) equed, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_equed);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slaqsb(void* uplo, void* n, void* kd, void* ab, void* ldab, void* s, void* scond, void* amax, void* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed) __attribute__((alias("flexiblas_real_slaqsb_")));
#else
void flexiblas_real_slaqsb(void* uplo, void* n, void* kd, void* ab, void* ldab, void* s, void* scond, void* amax, void* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed){flexiblas_real_slaqsb_((void*) uplo, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) s, (void*) scond, (void*) amax, (void*) equed, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_equed);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slaqsb_(void* uplo, void* n, void* kd, void* ab, void* ldab, void* s, void* scond, void* amax, void* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed)
{
    void (*fn) (void* uplo, void* n, void* kd, void* ab, void* ldab, void* s, void* scond, void* amax, void* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);
    void (*fn_hook) (void* uplo, void* n, void* kd, void* ab, void* ldab, void* s, void* scond, void* amax, void* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    *(void **) &fn      = current_backend->lapack.slaqsb.f77_blas_function;

    hook_pos_slaqsb ++;
    if( hook_pos_slaqsb < __flexiblas_hooks->slaqsb.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slaqsb.f77_hook_function[hook_pos_slaqsb];
        fn_hook((void*) uplo, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) s, (void*) scond, (void*) amax, (void*) equed, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_equed);
    } else {
        hook_pos_slaqsb = 0;
        fn((void*) uplo, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) s, (void*) scond, (void*) amax, (void*) equed, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_equed);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slaqsb(void* uplo, void* n, void* kd, void* ab, void* ldab, void* s, void* scond, void* amax, void* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed) __attribute__((alias("flexiblas_chain_slaqsb_")));
#else
void flexiblas_chain_slaqsb(void* uplo, void* n, void* kd, void* ab, void* ldab, void* s, void* scond, void* amax, void* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed){flexiblas_chain_slaqsb_((void*) uplo, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) s, (void*) scond, (void*) amax, (void*) equed, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_equed);}
#endif



