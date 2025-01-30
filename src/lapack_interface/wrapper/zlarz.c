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


static TLS_STORE uint8_t hook_pos_zlarz = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlarz,ZLARZ)(char* side, blasint* m, blasint* n, blasint* l, double complex* v, blasint* incv, double complex* tau, double complex* c, blasint* ldc, double complex* work, flexiblas_fortran_charlen_t len_side)
#else
void FC_GLOBAL(zlarz,ZLARZ)(char* side, blasint* m, blasint* n, blasint* l, double complex* v, blasint* incv, double complex* tau, double complex* c, blasint* ldc, double complex* work, flexiblas_fortran_charlen_t len_side)
#endif
{
    void (*fn) (void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);
    void (*fn_hook) (void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlarz.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlarz.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side);
        return;
    } else {
        hook_pos_zlarz = 0;
        fn_hook((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zlarz,ZLARZ)(char* side, blasint* m, blasint* n, blasint* l, double complex* v, blasint* incv, double complex* tau, double complex* c, blasint* ldc, double complex* work, flexiblas_fortran_charlen_t len_side) __attribute__((alias(MTS(FC_GLOBAL(zlarz,ZLARZ)))));
void FC_GLOBAL3(zlarz,ZLARZ)(char* side, blasint* m, blasint* n, blasint* l, double complex* v, blasint* incv, double complex* tau, double complex* c, blasint* ldc, double complex* work, flexiblas_fortran_charlen_t len_side) __attribute__((alias(MTS(FC_GLOBAL(zlarz,ZLARZ)))));
#else
void FC_GLOBAL2(zlarz,ZLARZ)(char* side, blasint* m, blasint* n, blasint* l, double complex* v, blasint* incv, double complex* tau, double complex* c, blasint* ldc, double complex* work, flexiblas_fortran_charlen_t len_side){ FC_GLOBAL(zlarz,ZLARZ)((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work, (flexiblas_fortran_charlen_t) len_side); }
void FC_GLOBAL3(zlarz,ZLARZ)(char* side, blasint* m, blasint* n, blasint* l, double complex* v, blasint* incv, double complex* tau, double complex* c, blasint* ldc, double complex* work, flexiblas_fortran_charlen_t len_side){ FC_GLOBAL(zlarz,ZLARZ)((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work, (flexiblas_fortran_charlen_t) len_side); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlarz_(void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side)
{
    void (*fn) (void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);

    *(void **) & fn = current_backend->lapack.zlarz.f77_blas_function;

    fn((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlarz(void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side) __attribute__((alias("flexiblas_real_zlarz_")));
#else
void flexiblas_real_zlarz(void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side){flexiblas_real_zlarz_((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work, (flexiblas_fortran_charlen_t) len_side);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlarz_(void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side)
{
    void (*fn) (void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);
    void (*fn_hook) (void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);

    *(void **) &fn      = current_backend->lapack.zlarz.f77_blas_function;

    hook_pos_zlarz ++;
    if( hook_pos_zlarz < __flexiblas_hooks->zlarz.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlarz.f77_hook_function[hook_pos_zlarz];
        fn_hook((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side);
    } else {
        hook_pos_zlarz = 0;
        fn((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlarz(void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side) __attribute__((alias("flexiblas_chain_zlarz_")));
#else
void flexiblas_chain_zlarz(void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side){flexiblas_chain_zlarz_((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work, (flexiblas_fortran_charlen_t) len_side);}
#endif



