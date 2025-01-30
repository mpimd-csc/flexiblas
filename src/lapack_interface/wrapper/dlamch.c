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


static TLS_STORE uint8_t hook_pos_dlamch = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dlamch,DLAMCH)(char* cmach, flexiblas_fortran_charlen_t len_cmach)
#else
double FC_GLOBAL(dlamch,DLAMCH)(char* cmach, flexiblas_fortran_charlen_t len_cmach)
#endif
{
    double (*fn) (void* cmach, flexiblas_fortran_charlen_t len_cmach);
    double (*fn_hook) (void* cmach, flexiblas_fortran_charlen_t len_cmach);
    double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlamch.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlamch.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) cmach, ( flexiblas_fortran_charlen_t ) len_cmach);
        return ret;
    } else {
        hook_pos_dlamch = 0;
        ret = fn_hook((void*) cmach, ( flexiblas_fortran_charlen_t ) len_cmach);
        return ret;
    }
}
#ifndef __APPLE__
double FC_GLOBAL2(dlamch,DLAMCH)(char* cmach, flexiblas_fortran_charlen_t len_cmach) __attribute__((alias(MTS(FC_GLOBAL(dlamch,DLAMCH)))));
double FC_GLOBAL3(dlamch,DLAMCH)(char* cmach, flexiblas_fortran_charlen_t len_cmach) __attribute__((alias(MTS(FC_GLOBAL(dlamch,DLAMCH)))));
#else
double FC_GLOBAL2(dlamch,DLAMCH)(char* cmach, flexiblas_fortran_charlen_t len_cmach){ return FC_GLOBAL(dlamch,DLAMCH)((void*) cmach, (flexiblas_fortran_charlen_t) len_cmach); }
double FC_GLOBAL3(dlamch,DLAMCH)(char* cmach, flexiblas_fortran_charlen_t len_cmach){ return FC_GLOBAL(dlamch,DLAMCH)((void*) cmach, (flexiblas_fortran_charlen_t) len_cmach); }
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dlamch_(void* cmach, flexiblas_fortran_charlen_t len_cmach)
{
    double (*fn) (void* cmach, flexiblas_fortran_charlen_t len_cmach);
    double ret;

    *(void **) & fn = current_backend->lapack.dlamch.f77_blas_function;

    ret = fn((void*) cmach, ( flexiblas_fortran_charlen_t ) len_cmach);

    return ret;
}
#ifndef __APPLE__
double flexiblas_real_dlamch(void* cmach, flexiblas_fortran_charlen_t len_cmach) __attribute__((alias("flexiblas_real_dlamch_")));
#else
double flexiblas_real_dlamch(void* cmach, flexiblas_fortran_charlen_t len_cmach){return flexiblas_real_dlamch_((void*) cmach, (flexiblas_fortran_charlen_t) len_cmach);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_dlamch_(void* cmach, flexiblas_fortran_charlen_t len_cmach)
{
    double (*fn) (void* cmach, flexiblas_fortran_charlen_t len_cmach);
    double (*fn_hook) (void* cmach, flexiblas_fortran_charlen_t len_cmach);
    double ret;

    *(void **) &fn      = current_backend->lapack.dlamch.f77_blas_function;

    hook_pos_dlamch ++;
    if( hook_pos_dlamch < __flexiblas_hooks->dlamch.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlamch.f77_hook_function[hook_pos_dlamch];
        ret = fn_hook((void*) cmach, ( flexiblas_fortran_charlen_t )len_cmach);
    } else {
        hook_pos_dlamch = 0;
        ret = fn((void*) cmach, ( flexiblas_fortran_charlen_t ) len_cmach);
    }
    return ret;
}
#ifndef __APPLE__
double flexiblas_chain_dlamch(void* cmach, flexiblas_fortran_charlen_t len_cmach) __attribute__((alias("flexiblas_chain_dlamch_")));
#else
double flexiblas_chain_dlamch(void* cmach, flexiblas_fortran_charlen_t len_cmach){return flexiblas_chain_dlamch_((void*) cmach, (flexiblas_fortran_charlen_t) len_cmach);}
#endif



