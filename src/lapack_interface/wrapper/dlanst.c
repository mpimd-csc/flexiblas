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


static TLS_STORE uint8_t hook_pos_dlanst = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dlanst,DLANST)(char* norm, blasint* n, double* d, double* e, flexiblas_fortran_charlen_t len_norm)
#else
double FC_GLOBAL(dlanst,DLANST)(char* norm, blasint* n, double* d, double* e, flexiblas_fortran_charlen_t len_norm)
#endif
{
    double (*fn) (void* norm, void* n, void* d, void* e, flexiblas_fortran_charlen_t len_norm);
    double (*fn_hook) (void* norm, void* n, void* d, void* e, flexiblas_fortran_charlen_t len_norm);
    double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlanst.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlanst.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) norm, (void*) n, (void*) d, (void*) e, ( flexiblas_fortran_charlen_t ) len_norm);
        return ret;
    } else {
        hook_pos_dlanst = 0;
        ret = fn_hook((void*) norm, (void*) n, (void*) d, (void*) e, ( flexiblas_fortran_charlen_t ) len_norm);
        return ret;
    }
}
#ifndef __APPLE__
double FC_GLOBAL2(dlanst,DLANST)(char* norm, blasint* n, double* d, double* e, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(dlanst,DLANST)))));
double FC_GLOBAL3(dlanst,DLANST)(char* norm, blasint* n, double* d, double* e, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(dlanst,DLANST)))));
#else
double FC_GLOBAL2(dlanst,DLANST)(char* norm, blasint* n, double* d, double* e, flexiblas_fortran_charlen_t len_norm){ return FC_GLOBAL(dlanst,DLANST)((void*) norm, (void*) n, (void*) d, (void*) e, (flexiblas_fortran_charlen_t) len_norm); }
double FC_GLOBAL3(dlanst,DLANST)(char* norm, blasint* n, double* d, double* e, flexiblas_fortran_charlen_t len_norm){ return FC_GLOBAL(dlanst,DLANST)((void*) norm, (void*) n, (void*) d, (void*) e, (flexiblas_fortran_charlen_t) len_norm); }
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dlanst_(void* norm, void* n, void* d, void* e, flexiblas_fortran_charlen_t len_norm)
{
    double (*fn) (void* norm, void* n, void* d, void* e, flexiblas_fortran_charlen_t len_norm);
    double ret;

    *(void **) & fn = current_backend->lapack.dlanst.f77_blas_function;

    ret = fn((void*) norm, (void*) n, (void*) d, (void*) e, ( flexiblas_fortran_charlen_t ) len_norm);

    return ret;
}
#ifndef __APPLE__
double flexiblas_real_dlanst(void* norm, void* n, void* d, void* e, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_real_dlanst_")));
#else
double flexiblas_real_dlanst(void* norm, void* n, void* d, void* e, flexiblas_fortran_charlen_t len_norm){return flexiblas_real_dlanst_((void*) norm, (void*) n, (void*) d, (void*) e, (flexiblas_fortran_charlen_t) len_norm);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_dlanst_(void* norm, void* n, void* d, void* e, flexiblas_fortran_charlen_t len_norm)
{
    double (*fn) (void* norm, void* n, void* d, void* e, flexiblas_fortran_charlen_t len_norm);
    double (*fn_hook) (void* norm, void* n, void* d, void* e, flexiblas_fortran_charlen_t len_norm);
    double ret;

    *(void **) &fn      = current_backend->lapack.dlanst.f77_blas_function;

    hook_pos_dlanst ++;
    if( hook_pos_dlanst < __flexiblas_hooks->dlanst.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlanst.f77_hook_function[hook_pos_dlanst];
        ret = fn_hook((void*) norm, (void*) n, (void*) d, (void*) e, ( flexiblas_fortran_charlen_t )len_norm);
    } else {
        hook_pos_dlanst = 0;
        ret = fn((void*) norm, (void*) n, (void*) d, (void*) e, ( flexiblas_fortran_charlen_t ) len_norm);
    }
    return ret;
}
#ifndef __APPLE__
double flexiblas_chain_dlanst(void* norm, void* n, void* d, void* e, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_chain_dlanst_")));
#else
double flexiblas_chain_dlanst(void* norm, void* n, void* d, void* e, flexiblas_fortran_charlen_t len_norm){return flexiblas_chain_dlanst_((void*) norm, (void*) n, (void*) d, (void*) e, (flexiblas_fortran_charlen_t) len_norm);}
#endif



