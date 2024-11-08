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


static TLS_STORE uint8_t hook_pos_dlabad = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlabad,DLABAD)(double* small, double* large)
#else
void FC_GLOBAL(dlabad,DLABAD)(double* small, double* large)
#endif
{
    void (*fn) (void* small, void* large);
    void (*fn_hook) (void* small, void* large);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlabad.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlabad.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) small, (void*) large);
        return;
    } else {
        hook_pos_dlabad = 0;
        fn_hook((void*) small, (void*) large);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dlabad_(double* small, double* large) __attribute__((alias(MTS(FC_GLOBAL(dlabad,DLABAD)))));
#else
#ifndef __APPLE__
void dlabad(double* small, double* large) __attribute__((alias(MTS(FC_GLOBAL(dlabad,DLABAD)))));
#else
void dlabad(double* small, double* large){ FC_GLOBAL(dlabad,DLABAD)((void*) small, (void*) large); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlabad_(void* small, void* large)
{
    void (*fn) (void* small, void* large);

    *(void **) & fn = current_backend->lapack.dlabad.f77_blas_function;

    fn((void*) small, (void*) large);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlabad(void* small, void* large) __attribute__((alias("flexiblas_real_dlabad_")));
#else
void flexiblas_real_dlabad(void* small, void* large){flexiblas_real_dlabad_((void*) small, (void*) large);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlabad_(void* small, void* large)
{
    void (*fn) (void* small, void* large);
    void (*fn_hook) (void* small, void* large);

    *(void **) &fn      = current_backend->lapack.dlabad.f77_blas_function;

    hook_pos_dlabad ++;
    if( hook_pos_dlabad < __flexiblas_hooks->dlabad.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlabad.f77_hook_function[hook_pos_dlabad];
        fn_hook((void*) small, (void*) large);
    } else {
        hook_pos_dlabad = 0;
        fn((void*) small, (void*) large);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlabad(void* small, void* large) __attribute__((alias("flexiblas_chain_dlabad_")));
#else
void flexiblas_chain_dlabad(void* small, void* large){flexiblas_chain_dlabad_((void*) small, (void*) large);}
#endif



