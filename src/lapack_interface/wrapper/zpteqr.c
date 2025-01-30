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


static TLS_STORE uint8_t hook_pos_zpteqr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zpteqr,ZPTEQR)(char* compz, blasint* n, double* d, double* e, double complex* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_compz)
#else
void FC_GLOBAL(zpteqr,ZPTEQR)(char* compz, blasint* n, double* d, double* e, double complex* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_compz)
#endif
{
    void (*fn) (void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz);
    void (*fn_hook) (void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zpteqr.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zpteqr.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compz);
        return;
    } else {
        hook_pos_zpteqr = 0;
        fn_hook((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compz);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zpteqr,ZPTEQR)(char* compz, blasint* n, double* d, double* e, double complex* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_compz) __attribute__((alias(MTS(FC_GLOBAL(zpteqr,ZPTEQR)))));
void FC_GLOBAL3(zpteqr,ZPTEQR)(char* compz, blasint* n, double* d, double* e, double complex* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_compz) __attribute__((alias(MTS(FC_GLOBAL(zpteqr,ZPTEQR)))));
#else
void FC_GLOBAL2(zpteqr,ZPTEQR)(char* compz, blasint* n, double* d, double* e, double complex* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_compz){ FC_GLOBAL(zpteqr,ZPTEQR)((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_compz); }
void FC_GLOBAL3(zpteqr,ZPTEQR)(char* compz, blasint* n, double* d, double* e, double complex* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_compz){ FC_GLOBAL(zpteqr,ZPTEQR)((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_compz); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zpteqr_(void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz)
{
    void (*fn) (void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz);

    *(void **) & fn = current_backend->lapack.zpteqr.f77_blas_function;

    fn((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compz);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zpteqr(void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz) __attribute__((alias("flexiblas_real_zpteqr_")));
#else
void flexiblas_real_zpteqr(void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz){flexiblas_real_zpteqr_((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_compz);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zpteqr_(void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz)
{
    void (*fn) (void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz);
    void (*fn_hook) (void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz);

    *(void **) &fn      = current_backend->lapack.zpteqr.f77_blas_function;

    hook_pos_zpteqr ++;
    if( hook_pos_zpteqr < __flexiblas_hooks->zpteqr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zpteqr.f77_hook_function[hook_pos_zpteqr];
        fn_hook((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compz);
    } else {
        hook_pos_zpteqr = 0;
        fn((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compz);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zpteqr(void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz) __attribute__((alias("flexiblas_chain_zpteqr_")));
#else
void flexiblas_chain_zpteqr(void* compz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* info, flexiblas_fortran_charlen_t len_compz){flexiblas_chain_zpteqr_((void*) compz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_compz);}
#endif



