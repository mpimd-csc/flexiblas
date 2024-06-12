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


static TLS_STORE uint8_t hook_pos_second = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(second,SECOND)(void)
#else
float FC_GLOBAL(second,SECOND)(void)
#endif
{
    float (*fn) (void);
    float (*fn_hook) (void);
    float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.second.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->second.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn();
        return ret;
    } else {
        hook_pos_second = 0;
        ret = fn_hook();
        return ret;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
float second_(void) __attribute__((alias(MTS(FC_GLOBAL(second,SECOND)))));
#else
#ifndef __APPLE__
float second(void) __attribute__((alias(MTS(FC_GLOBAL(second,SECOND)))));
#else
float second(void){ return FC_GLOBAL(second,SECOND)(); }
#endif
#endif




/* Real Implementation for Hooks */


float flexiblas_real_second_(void)
{
    float (*fn) (void);
    float ret;

    *(void **) & fn = current_backend->lapack.second.f77_blas_function;

    ret = fn();

    return ret;
}
#ifndef __APPLE__
float flexiblas_real_second(void) __attribute__((alias("flexiblas_real_second_")));
#else
float flexiblas_real_second(void){return flexiblas_real_second_();}
#endif




/* Chainloader for Hooks */


float flexiblas_chain_second_(void)
{
    float (*fn) (void);
    float (*fn_hook) (void);
    float ret;

    *(void **) &fn      = current_backend->lapack.second.f77_blas_function;

    hook_pos_second ++;
    if( hook_pos_second < __flexiblas_hooks->second.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->second.f77_hook_function[hook_pos_second];
        ret = fn_hook();
    } else {
        hook_pos_second = 0;
        ret = fn();
    }
    return ret;
}
#ifndef __APPLE__
float flexiblas_chain_second(void) __attribute__((alias("flexiblas_chain_second_")));
#else
float flexiblas_chain_second(void){return flexiblas_chain_second_();}
#endif



