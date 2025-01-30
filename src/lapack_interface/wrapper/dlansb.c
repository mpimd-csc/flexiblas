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


static TLS_STORE uint8_t hook_pos_dlansb = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dlansb,DLANSB)(char* norm, char* uplo, blasint* n, blasint* k, double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo)
#else
double FC_GLOBAL(dlansb,DLANSB)(char* norm, char* uplo, blasint* n, blasint* k, double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    double (*fn) (void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    double (*fn_hook) (void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlansb.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlansb.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo);
        return ret;
    } else {
        hook_pos_dlansb = 0;
        ret = fn_hook((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo);
        return ret;
    }
}
#ifndef __APPLE__
double FC_GLOBAL2(dlansb,DLANSB)(char* norm, char* uplo, blasint* n, blasint* k, double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(dlansb,DLANSB)))));
double FC_GLOBAL3(dlansb,DLANSB)(char* norm, char* uplo, blasint* n, blasint* k, double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(dlansb,DLANSB)))));
#else
double FC_GLOBAL2(dlansb,DLANSB)(char* norm, char* uplo, blasint* n, blasint* k, double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo){ return FC_GLOBAL(dlansb,DLANSB)((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo); }
double FC_GLOBAL3(dlansb,DLANSB)(char* norm, char* uplo, blasint* n, blasint* k, double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo){ return FC_GLOBAL(dlansb,DLANSB)((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo); }
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dlansb_(void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo)
{
    double (*fn) (void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    double ret;

    *(void **) & fn = current_backend->lapack.dlansb.f77_blas_function;

    ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo);

    return ret;
}
#ifndef __APPLE__
double flexiblas_real_dlansb(void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_dlansb_")));
#else
double flexiblas_real_dlansb(void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo){return flexiblas_real_dlansb_((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_dlansb_(void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo)
{
    double (*fn) (void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    double (*fn_hook) (void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    double ret;

    *(void **) &fn      = current_backend->lapack.dlansb.f77_blas_function;

    hook_pos_dlansb ++;
    if( hook_pos_dlansb < __flexiblas_hooks->dlansb.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlansb.f77_hook_function[hook_pos_dlansb];
        ret = fn_hook((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work, ( flexiblas_fortran_charlen_t )len_norm, ( flexiblas_fortran_charlen_t )len_uplo);
    } else {
        hook_pos_dlansb = 0;
        ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return ret;
}
#ifndef __APPLE__
double flexiblas_chain_dlansb(void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_dlansb_")));
#else
double flexiblas_chain_dlansb(void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo){return flexiblas_chain_dlansb_((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



