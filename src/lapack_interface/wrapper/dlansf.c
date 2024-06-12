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


static TLS_STORE uint8_t hook_pos_dlansf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dlansf,DLANSF)(char* norm, char* transr, char* uplo, blasint* n, double* a, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
#else
double FC_GLOBAL(dlansf,DLANSF)(char* norm, char* transr, char* uplo, blasint* n, double* a, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    double (*fn) (void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
    double (*fn_hook) (void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
    double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlansf.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlansf.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
        return ret;
    } else {
        hook_pos_dlansf = 0;
        ret = fn_hook((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
        return ret;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
double dlansf_(char* norm, char* transr, char* uplo, blasint* n, double* a, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(dlansf,DLANSF)))));
#else
#ifndef __APPLE__
double dlansf(char* norm, char* transr, char* uplo, blasint* n, double* a, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(dlansf,DLANSF)))));
#else
double dlansf(char* norm, char* transr, char* uplo, blasint* n, double* a, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){ return FC_GLOBAL(dlansf,DLANSF)((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo); }
#endif
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dlansf_(void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
{
    double (*fn) (void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
    double ret;

    *(void **) & fn = current_backend->lapack.dlansf.f77_blas_function;

    ret = fn((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);

    return ret;
}
#ifndef __APPLE__
double flexiblas_real_dlansf(void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_dlansf_")));
#else
double flexiblas_real_dlansf(void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){return flexiblas_real_dlansf_((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_dlansf_(void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
{
    double (*fn) (void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
    double (*fn_hook) (void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
    double ret;

    *(void **) &fn      = current_backend->lapack.dlansf.f77_blas_function;

    hook_pos_dlansf ++;
    if( hook_pos_dlansf < __flexiblas_hooks->dlansf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlansf.f77_hook_function[hook_pos_dlansf];
        ret = fn_hook((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, ( flexiblas_fortran_charlen_t )len_norm, ( flexiblas_fortran_charlen_t )len_transr, ( flexiblas_fortran_charlen_t )len_uplo);
    } else {
        hook_pos_dlansf = 0;
        ret = fn((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return ret;
}
#ifndef __APPLE__
double flexiblas_chain_dlansf(void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_dlansf_")));
#else
double flexiblas_chain_dlansf(void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){return flexiblas_chain_dlansf_((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



