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


static TLS_STORE uint8_t hook_pos_clanhe = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(clanhe,CLANHE)(char* norm, char* uplo, blasint* n, float complex* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo)
#else
float FC_GLOBAL(clanhe,CLANHE)(char* norm, char* uplo, blasint* n, float complex* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    float (*fn) (void* norm, void* uplo, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    float (*fn_hook) (void* norm, void* uplo, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.clanhe.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->clanhe.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo);
        return ret;
    } else {
        hook_pos_clanhe = 0;
        ret = fn_hook((void*) norm, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo);
        return ret;
    }
}
#ifndef __APPLE__
float FC_GLOBAL2(clanhe,CLANHE)(char* norm, char* uplo, blasint* n, float complex* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(clanhe,CLANHE)))));
float FC_GLOBAL3(clanhe,CLANHE)(char* norm, char* uplo, blasint* n, float complex* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(clanhe,CLANHE)))));
#else
float FC_GLOBAL2(clanhe,CLANHE)(char* norm, char* uplo, blasint* n, float complex* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo){ return FC_GLOBAL(clanhe,CLANHE)((void*) norm, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo); }
float FC_GLOBAL3(clanhe,CLANHE)(char* norm, char* uplo, blasint* n, float complex* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo){ return FC_GLOBAL(clanhe,CLANHE)((void*) norm, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo); }
#endif




/* Real Implementation for Hooks */


float flexiblas_real_clanhe_(void* norm, void* uplo, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo)
{
    float (*fn) (void* norm, void* uplo, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    float ret;

    *(void **) & fn = current_backend->lapack.clanhe.f77_blas_function;

    ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo);

    return ret;
}
#ifndef __APPLE__
float flexiblas_real_clanhe(void* norm, void* uplo, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_clanhe_")));
#else
float flexiblas_real_clanhe(void* norm, void* uplo, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo){return flexiblas_real_clanhe_((void*) norm, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


float flexiblas_chain_clanhe_(void* norm, void* uplo, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo)
{
    float (*fn) (void* norm, void* uplo, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    float (*fn_hook) (void* norm, void* uplo, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);
    float ret;

    *(void **) &fn      = current_backend->lapack.clanhe.f77_blas_function;

    hook_pos_clanhe ++;
    if( hook_pos_clanhe < __flexiblas_hooks->clanhe.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clanhe.f77_hook_function[hook_pos_clanhe];
        ret = fn_hook((void*) norm, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t )len_norm, ( flexiblas_fortran_charlen_t )len_uplo);
    } else {
        hook_pos_clanhe = 0;
        ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return ret;
}
#ifndef __APPLE__
float flexiblas_chain_clanhe(void* norm, void* uplo, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_clanhe_")));
#else
float flexiblas_chain_clanhe(void* norm, void* uplo, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo){return flexiblas_chain_clanhe_((void*) norm, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



