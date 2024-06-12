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


static TLS_STORE uint8_t hook_pos_ilatrans = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(ilatrans,ILATRANS)(char* trans, flexiblas_fortran_charlen_t len_trans)
#else
int FC_GLOBAL(ilatrans,ILATRANS)(char* trans, flexiblas_fortran_charlen_t len_trans)
#endif
{
    blasint (*fn) (void* trans, flexiblas_fortran_charlen_t len_trans);
    blasint (*fn_hook) (void* trans, flexiblas_fortran_charlen_t len_trans);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilatrans.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilatrans.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) trans, ( flexiblas_fortran_charlen_t ) len_trans);
        return ret;
    } else {
        hook_pos_ilatrans = 0;
        ret = fn_hook((void*) trans, ( flexiblas_fortran_charlen_t ) len_trans);
        return ret;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
int ilatrans_(char* trans, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(ilatrans,ILATRANS)))));
#else
#ifndef __APPLE__
int ilatrans(char* trans, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(ilatrans,ILATRANS)))));
#else
int ilatrans(char* trans, flexiblas_fortran_charlen_t len_trans){ return FC_GLOBAL(ilatrans,ILATRANS)((void*) trans, (flexiblas_fortran_charlen_t) len_trans); }
#endif
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilatrans_(void* trans, flexiblas_fortran_charlen_t len_trans)
{
    blasint (*fn) (void* trans, flexiblas_fortran_charlen_t len_trans);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilatrans.f77_blas_function;

    ret = fn((void*) trans, ( flexiblas_fortran_charlen_t ) len_trans);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilatrans(void* trans, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_ilatrans_")));
#else
blasint flexiblas_real_ilatrans(void* trans, flexiblas_fortran_charlen_t len_trans){return flexiblas_real_ilatrans_((void*) trans, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilatrans_(void* trans, flexiblas_fortran_charlen_t len_trans)
{
    blasint (*fn) (void* trans, flexiblas_fortran_charlen_t len_trans);
    blasint (*fn_hook) (void* trans, flexiblas_fortran_charlen_t len_trans);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilatrans.f77_blas_function;

    hook_pos_ilatrans ++;
    if( hook_pos_ilatrans < __flexiblas_hooks->ilatrans.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilatrans.f77_hook_function[hook_pos_ilatrans];
        ret = fn_hook((void*) trans, ( flexiblas_fortran_charlen_t )len_trans);
    } else {
        hook_pos_ilatrans = 0;
        ret = fn((void*) trans, ( flexiblas_fortran_charlen_t ) len_trans);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilatrans(void* trans, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_ilatrans_")));
#else
blasint flexiblas_chain_ilatrans(void* trans, flexiblas_fortran_charlen_t len_trans){return flexiblas_chain_ilatrans_((void*) trans, (flexiblas_fortran_charlen_t) len_trans);}
#endif



