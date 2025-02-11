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


static TLS_STORE uint8_t hook_pos_xerbla_array = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL_(xerbla_array,XERBLA_ARRAY)(char* srname_array, blasint* srname_len, blasint* info, flexiblas_fortran_charlen_t len_srname_array)
#else
void FC_GLOBAL_(xerbla_array,XERBLA_ARRAY)(char* srname_array, blasint* srname_len, blasint* info, flexiblas_fortran_charlen_t len_srname_array)
#endif
{
    void (*fn) (void* srname_array, void* srname_len, void* info, flexiblas_fortran_charlen_t len_srname_array);
    void (*fn_hook) (void* srname_array, void* srname_len, void* info, flexiblas_fortran_charlen_t len_srname_array);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.xerbla_array.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->xerbla_array.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) srname_array, (void*) srname_len, (void*) info, ( flexiblas_fortran_charlen_t ) len_srname_array);
        return;
    } else {
        hook_pos_xerbla_array = 0;
        fn_hook((void*) srname_array, (void*) srname_len, (void*) info, ( flexiblas_fortran_charlen_t ) len_srname_array);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2_(xerbla_array,XERBLA_ARRAY)(char* srname_array, blasint* srname_len, blasint* info, flexiblas_fortran_charlen_t len_srname_array) __attribute__((alias(MTS(FC_GLOBAL_(xerbla_array,XERBLA_ARRAY)))));
void FC_GLOBAL3_(xerbla_array,XERBLA_ARRAY)(char* srname_array, blasint* srname_len, blasint* info, flexiblas_fortran_charlen_t len_srname_array) __attribute__((alias(MTS(FC_GLOBAL_(xerbla_array,XERBLA_ARRAY)))));
#else
void FC_GLOBAL2_(xerbla_array,XERBLA_ARRAY)(char* srname_array, blasint* srname_len, blasint* info, flexiblas_fortran_charlen_t len_srname_array){ FC_GLOBAL_(xerbla_array,XERBLA_ARRAY)((void*) srname_array, (void*) srname_len, (void*) info, (flexiblas_fortran_charlen_t) len_srname_array); }
void FC_GLOBAL3_(xerbla_array,XERBLA_ARRAY)(char* srname_array, blasint* srname_len, blasint* info, flexiblas_fortran_charlen_t len_srname_array){ FC_GLOBAL_(xerbla_array,XERBLA_ARRAY)((void*) srname_array, (void*) srname_len, (void*) info, (flexiblas_fortran_charlen_t) len_srname_array); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_xerbla_array_(void* srname_array, void* srname_len, void* info, flexiblas_fortran_charlen_t len_srname_array)
{
    void (*fn) (void* srname_array, void* srname_len, void* info, flexiblas_fortran_charlen_t len_srname_array);

    *(void **) & fn = current_backend->lapack.xerbla_array.f77_blas_function;

    fn((void*) srname_array, (void*) srname_len, (void*) info, ( flexiblas_fortran_charlen_t ) len_srname_array);

    return;
}
#ifndef __APPLE__
void flexiblas_real_xerbla_array(void* srname_array, void* srname_len, void* info, flexiblas_fortran_charlen_t len_srname_array) __attribute__((alias("flexiblas_real_xerbla_array_")));
#else
void flexiblas_real_xerbla_array(void* srname_array, void* srname_len, void* info, flexiblas_fortran_charlen_t len_srname_array){flexiblas_real_xerbla_array_((void*) srname_array, (void*) srname_len, (void*) info, (flexiblas_fortran_charlen_t) len_srname_array);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_xerbla_array_(void* srname_array, void* srname_len, void* info, flexiblas_fortran_charlen_t len_srname_array)
{
    void (*fn) (void* srname_array, void* srname_len, void* info, flexiblas_fortran_charlen_t len_srname_array);
    void (*fn_hook) (void* srname_array, void* srname_len, void* info, flexiblas_fortran_charlen_t len_srname_array);

    *(void **) &fn      = current_backend->lapack.xerbla_array.f77_blas_function;

    hook_pos_xerbla_array ++;
    if( hook_pos_xerbla_array < __flexiblas_hooks->xerbla_array.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->xerbla_array.f77_hook_function[hook_pos_xerbla_array];
        fn_hook((void*) srname_array, (void*) srname_len, (void*) info, ( flexiblas_fortran_charlen_t ) len_srname_array);
    } else {
        hook_pos_xerbla_array = 0;
        fn((void*) srname_array, (void*) srname_len, (void*) info, ( flexiblas_fortran_charlen_t ) len_srname_array);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_xerbla_array(void* srname_array, void* srname_len, void* info, flexiblas_fortran_charlen_t len_srname_array) __attribute__((alias("flexiblas_chain_xerbla_array_")));
#else
void flexiblas_chain_xerbla_array(void* srname_array, void* srname_len, void* info, flexiblas_fortran_charlen_t len_srname_array){flexiblas_chain_xerbla_array_((void*) srname_array, (void*) srname_len, (void*) info, (flexiblas_fortran_charlen_t) len_srname_array);}
#endif



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
#ifndef __APPLE__
void FC_GLOBAL2(xerbla,XERBLA)(char* srname, blasint* info, flexiblas_fortran_charlen_t len_srname) __attribute__((alias(MTS(FC_GLOBAL(xerbla,XERBLA)))));
void FC_GLOBAL3(xerbla,XERBLA)(char* srname, blasint* info, flexiblas_fortran_charlen_t len_srname) __attribute__((alias(MTS(FC_GLOBAL(xerbla,XERBLA)))));
#else
void FC_GLOBAL2(xerbla,XERBLA)(char* srname, blasint* info, flexiblas_fortran_charlen_t len_srname){ FC_GLOBAL(xerbla,XERBLA)((void*) srname, (void*) info, (flexiblas_fortran_charlen_t) len_srname); }
void FC_GLOBAL3(xerbla,XERBLA)(char* srname, blasint* info, flexiblas_fortran_charlen_t len_srname){ FC_GLOBAL(xerbla,XERBLA)((void*) srname, (void*) info, (flexiblas_fortran_charlen_t) len_srname); }
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



