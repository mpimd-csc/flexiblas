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


static TLS_STORE uint8_t hook_pos_xerbla = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(xerbla,XERBLA)(char* srname, blasint* info, flexiblas_fortran_charlen_t len_srname)
#else
void FC_GLOBAL(xerbla,XERBLA)(char* srname, blasint* info, flexiblas_fortran_charlen_t len_srname)
#endif
{
    void (*fn) (void* srname, void* info, flexiblas_fortran_charlen_t len_srname);
    void (*fn_hook) (void* srname, void* info, flexiblas_fortran_charlen_t len_srname);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.xerbla.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->xerbla.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) srname, (void*) info, ( flexiblas_fortran_charlen_t ) len_srname);
        return;
    } else {
        hook_pos_xerbla = 0;
        fn_hook((void*) srname, (void*) info, ( flexiblas_fortran_charlen_t ) len_srname);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void xerbla_(char* srname, blasint* info, flexiblas_fortran_charlen_t len_srname) __attribute__((alias(MTS(FC_GLOBAL(xerbla,XERBLA)))));
#else
#ifndef __APPLE__
void xerbla(char* srname, blasint* info, flexiblas_fortran_charlen_t len_srname) __attribute__((alias(MTS(FC_GLOBAL(xerbla,XERBLA)))));
#else
void xerbla(char* srname, blasint* info, flexiblas_fortran_charlen_t len_srname){ FC_GLOBAL(xerbla,XERBLA)((void*) srname, (void*) info, (flexiblas_fortran_charlen_t) len_srname); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_xerbla_(void* srname, void* info, flexiblas_fortran_charlen_t len_srname)
{
    void (*fn) (void* srname, void* info, flexiblas_fortran_charlen_t len_srname);

    *(void **) & fn = current_backend->lapack.xerbla.f77_blas_function;

    fn((void*) srname, (void*) info, ( flexiblas_fortran_charlen_t ) len_srname);

    return;
}
#ifndef __APPLE__
void flexiblas_real_xerbla(void* srname, void* info, flexiblas_fortran_charlen_t len_srname) __attribute__((alias("flexiblas_real_xerbla_")));
#else
void flexiblas_real_xerbla(void* srname, void* info, flexiblas_fortran_charlen_t len_srname){flexiblas_real_xerbla_((void*) srname, (void*) info, (flexiblas_fortran_charlen_t) len_srname);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_xerbla_(void* srname, void* info, flexiblas_fortran_charlen_t len_srname)
{
    void (*fn) (void* srname, void* info, flexiblas_fortran_charlen_t len_srname);
    void (*fn_hook) (void* srname, void* info, flexiblas_fortran_charlen_t len_srname);

    *(void **) &fn      = current_backend->lapack.xerbla.f77_blas_function;

    hook_pos_xerbla ++;
    if( hook_pos_xerbla < __flexiblas_hooks->xerbla.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->xerbla.f77_hook_function[hook_pos_xerbla];
        fn_hook((void*) srname, (void*) info, ( flexiblas_fortran_charlen_t ) len_srname);
    } else {
        hook_pos_xerbla = 0;
        fn((void*) srname, (void*) info, ( flexiblas_fortran_charlen_t ) len_srname);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_xerbla(void* srname, void* info, flexiblas_fortran_charlen_t len_srname) __attribute__((alias("flexiblas_chain_xerbla_")));
#else
void flexiblas_chain_xerbla(void* srname, void* info, flexiblas_fortran_charlen_t len_srname){flexiblas_chain_xerbla_((void*) srname, (void*) info, (flexiblas_fortran_charlen_t) len_srname);}
#endif



