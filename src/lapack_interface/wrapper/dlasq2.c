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


static TLS_STORE uint8_t hook_pos_dlasq2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlasq2,DLASQ2)(blasint* n, double* z, blasint* info)
#else
void FC_GLOBAL(dlasq2,DLASQ2)(blasint* n, double* z, blasint* info)
#endif
{
    void (*fn) (void* n, void* z, void* info);
    void (*fn_hook) (void* n, void* z, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlasq2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlasq2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) z, (void*) info);
        return;
    } else {
        hook_pos_dlasq2 = 0;
        fn_hook((void*) n, (void*) z, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlasq2,DLASQ2)(blasint* n, double* z, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasq2,DLASQ2)))));
void FC_GLOBAL3(dlasq2,DLASQ2)(blasint* n, double* z, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasq2,DLASQ2)))));
#else
void FC_GLOBAL2(dlasq2,DLASQ2)(blasint* n, double* z, blasint* info){ FC_GLOBAL(dlasq2,DLASQ2)((void*) n, (void*) z, (void*) info); }
void FC_GLOBAL3(dlasq2,DLASQ2)(blasint* n, double* z, blasint* info){ FC_GLOBAL(dlasq2,DLASQ2)((void*) n, (void*) z, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlasq2_(void* n, void* z, void* info)
{
    void (*fn) (void* n, void* z, void* info);

    *(void **) & fn = current_backend->lapack.dlasq2.f77_blas_function;

    fn((void*) n, (void*) z, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlasq2(void* n, void* z, void* info) __attribute__((alias("flexiblas_real_dlasq2_")));
#else
void flexiblas_real_dlasq2(void* n, void* z, void* info){flexiblas_real_dlasq2_((void*) n, (void*) z, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlasq2_(void* n, void* z, void* info)
{
    void (*fn) (void* n, void* z, void* info);
    void (*fn_hook) (void* n, void* z, void* info);

    *(void **) &fn      = current_backend->lapack.dlasq2.f77_blas_function;

    hook_pos_dlasq2 ++;
    if( hook_pos_dlasq2 < __flexiblas_hooks->dlasq2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlasq2.f77_hook_function[hook_pos_dlasq2];
        fn_hook((void*) n, (void*) z, (void*) info);
    } else {
        hook_pos_dlasq2 = 0;
        fn((void*) n, (void*) z, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlasq2(void* n, void* z, void* info) __attribute__((alias("flexiblas_chain_dlasq2_")));
#else
void flexiblas_chain_dlasq2(void* n, void* z, void* info){flexiblas_chain_dlasq2_((void*) n, (void*) z, (void*) info);}
#endif



