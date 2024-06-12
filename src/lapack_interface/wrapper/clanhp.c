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


static TLS_STORE uint8_t hook_pos_clanhp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(clanhp,CLANHP)(char* norm, char* uplo, blasint* n, float complex* ap, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo)
#else
float FC_GLOBAL(clanhp,CLANHP)(char* norm, char* uplo, blasint* n, float complex* ap, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    float (*fn) (void* norm, void* uplo, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    float (*fn_hook) (void* norm, void* uplo, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.clanhp.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->clanhp.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo);
        return ret;
    } else {
        hook_pos_clanhp = 0;
        ret = fn_hook((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo);
        return ret;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
float clanhp_(char* norm, char* uplo, blasint* n, float complex* ap, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(clanhp,CLANHP)))));
#else
#ifndef __APPLE__
float clanhp(char* norm, char* uplo, blasint* n, float complex* ap, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(clanhp,CLANHP)))));
#else
float clanhp(char* norm, char* uplo, blasint* n, float complex* ap, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo){ return FC_GLOBAL(clanhp,CLANHP)((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo); }
#endif
#endif




/* Real Implementation for Hooks */


float flexiblas_real_clanhp_(void* norm, void* uplo, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo)
{
    float (*fn) (void* norm, void* uplo, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    float ret;

    *(void **) & fn = current_backend->lapack.clanhp.f77_blas_function;

    ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo);

    return ret;
}
#ifndef __APPLE__
float flexiblas_real_clanhp(void* norm, void* uplo, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_clanhp_")));
#else
float flexiblas_real_clanhp(void* norm, void* uplo, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo){return flexiblas_real_clanhp_((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


float flexiblas_chain_clanhp_(void* norm, void* uplo, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo)
{
    float (*fn) (void* norm, void* uplo, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    float (*fn_hook) (void* norm, void* uplo, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    float ret;

    *(void **) &fn      = current_backend->lapack.clanhp.f77_blas_function;

    hook_pos_clanhp ++;
    if( hook_pos_clanhp < __flexiblas_hooks->clanhp.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clanhp.f77_hook_function[hook_pos_clanhp];
        ret = fn_hook((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work, ( flexiblas_fortran_charlen_t )len_norm, ( flexiblas_fortran_charlen_t )len_uplo);
    } else {
        hook_pos_clanhp = 0;
        ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return ret;
}
#ifndef __APPLE__
float flexiblas_chain_clanhp(void* norm, void* uplo, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_clanhp_")));
#else
float flexiblas_chain_clanhp(void* norm, void* uplo, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo){return flexiblas_chain_clanhp_((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



