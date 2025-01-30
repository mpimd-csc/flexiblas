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


static TLS_STORE uint8_t hook_pos_slabad = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slabad,SLABAD)(float* small, float* large)
#else
void FC_GLOBAL(slabad,SLABAD)(float* small, float* large)
#endif
{
    void (*fn) (void* small, void* large);
    void (*fn_hook) (void* small, void* large);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slabad.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slabad.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) small, (void*) large);
        return;
    } else {
        hook_pos_slabad = 0;
        fn_hook((void*) small, (void*) large);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slabad,SLABAD)(float* small, float* large) __attribute__((alias(MTS(FC_GLOBAL(slabad,SLABAD)))));
void FC_GLOBAL3(slabad,SLABAD)(float* small, float* large) __attribute__((alias(MTS(FC_GLOBAL(slabad,SLABAD)))));
#else
void FC_GLOBAL2(slabad,SLABAD)(float* small, float* large){ FC_GLOBAL(slabad,SLABAD)((void*) small, (void*) large); }
void FC_GLOBAL3(slabad,SLABAD)(float* small, float* large){ FC_GLOBAL(slabad,SLABAD)((void*) small, (void*) large); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slabad_(void* small, void* large)
{
    void (*fn) (void* small, void* large);

    *(void **) & fn = current_backend->lapack.slabad.f77_blas_function;

    fn((void*) small, (void*) large);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slabad(void* small, void* large) __attribute__((alias("flexiblas_real_slabad_")));
#else
void flexiblas_real_slabad(void* small, void* large){flexiblas_real_slabad_((void*) small, (void*) large);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slabad_(void* small, void* large)
{
    void (*fn) (void* small, void* large);
    void (*fn_hook) (void* small, void* large);

    *(void **) &fn      = current_backend->lapack.slabad.f77_blas_function;

    hook_pos_slabad ++;
    if( hook_pos_slabad < __flexiblas_hooks->slabad.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slabad.f77_hook_function[hook_pos_slabad];
        fn_hook((void*) small, (void*) large);
    } else {
        hook_pos_slabad = 0;
        fn((void*) small, (void*) large);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slabad(void* small, void* large) __attribute__((alias("flexiblas_chain_slabad_")));
#else
void flexiblas_chain_slabad(void* small, void* large){flexiblas_chain_slabad_((void*) small, (void*) large);}
#endif



