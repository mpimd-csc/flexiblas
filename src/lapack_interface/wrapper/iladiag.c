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


static TLS_STORE uint8_t hook_pos_iladiag = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag)
#else
blasint FC_GLOBAL(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag)
#endif
{
    blasint (*fn) (void* diag, flexiblas_fortran_charlen_t len_diag);
    blasint (*fn_hook) (void* diag, flexiblas_fortran_charlen_t len_diag);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.iladiag.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->iladiag.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) diag, ( flexiblas_fortran_charlen_t ) len_diag);
        return ret;
    } else {
        hook_pos_iladiag = 0;
        ret = fn_hook((void*) diag, ( flexiblas_fortran_charlen_t ) len_diag);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(iladiag,ILADIAG)))));
blasint FC_GLOBAL3(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(iladiag,ILADIAG)))));
#else
blasint FC_GLOBAL2(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag){ return FC_GLOBAL(iladiag,ILADIAG)((void*) diag, (flexiblas_fortran_charlen_t) len_diag); }
blasint FC_GLOBAL3(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag){ return FC_GLOBAL(iladiag,ILADIAG)((void*) diag, (flexiblas_fortran_charlen_t) len_diag); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_iladiag_(void* diag, flexiblas_fortran_charlen_t len_diag)
{
    blasint (*fn) (void* diag, flexiblas_fortran_charlen_t len_diag);
    blasint ret;

    *(void **) & fn = current_backend->lapack.iladiag.f77_blas_function;

    ret = fn((void*) diag, ( flexiblas_fortran_charlen_t ) len_diag);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_iladiag(void* diag, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_real_iladiag_")));
#else
blasint flexiblas_real_iladiag(void* diag, flexiblas_fortran_charlen_t len_diag){return flexiblas_real_iladiag_((void*) diag, (flexiblas_fortran_charlen_t) len_diag);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_iladiag_(void* diag, flexiblas_fortran_charlen_t len_diag)
{
    blasint (*fn) (void* diag, flexiblas_fortran_charlen_t len_diag);
    blasint (*fn_hook) (void* diag, flexiblas_fortran_charlen_t len_diag);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.iladiag.f77_blas_function;

    hook_pos_iladiag ++;
    if( hook_pos_iladiag < __flexiblas_hooks->iladiag.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->iladiag.f77_hook_function[hook_pos_iladiag];
        ret = fn_hook((void*) diag, ( flexiblas_fortran_charlen_t )len_diag);
    } else {
        hook_pos_iladiag = 0;
        ret = fn((void*) diag, ( flexiblas_fortran_charlen_t ) len_diag);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_iladiag(void* diag, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_chain_iladiag_")));
#else
blasint flexiblas_chain_iladiag(void* diag, flexiblas_fortran_charlen_t len_diag){return flexiblas_chain_iladiag_((void*) diag, (flexiblas_fortran_charlen_t) len_diag);}
#endif



