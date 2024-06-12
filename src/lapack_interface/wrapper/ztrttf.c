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


static TLS_STORE uint8_t hook_pos_ztrttf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ztrttf,ZTRTTF)(char* transr, char* uplo, blasint* n, double complex* a, blasint* lda, double complex* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(ztrttf,ZTRTTF)(char* transr, char* uplo, blasint* n, double complex* a, blasint* lda, double complex* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    void (*fn) (void* transr, void* uplo, void* n, void* a, void* lda, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* transr, void* uplo, void* n, void* a, void* lda, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ztrttf.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ztrttf.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) arf, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    } else {
        hook_pos_ztrttf = 0;
        fn_hook((void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) arf, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void ztrttf_(char* transr, char* uplo, blasint* n, double complex* a, blasint* lda, double complex* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(ztrttf,ZTRTTF)))));
#else
#ifndef __APPLE__
void ztrttf(char* transr, char* uplo, blasint* n, double complex* a, blasint* lda, double complex* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(ztrttf,ZTRTTF)))));
#else
void ztrttf(char* transr, char* uplo, blasint* n, double complex* a, blasint* lda, double complex* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(ztrttf,ZTRTTF)((void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) arf, (void*) info, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ztrttf_(void* transr, void* uplo, void* n, void* a, void* lda, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* transr, void* uplo, void* n, void* a, void* lda, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    *(void **) & fn = current_backend->lapack.ztrttf.f77_blas_function;

    fn((void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) arf, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);

    return;
}
#ifndef __APPLE__
void flexiblas_real_ztrttf(void* transr, void* uplo, void* n, void* a, void* lda, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_ztrttf_")));
#else
void flexiblas_real_ztrttf(void* transr, void* uplo, void* n, void* a, void* lda, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_ztrttf_((void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) arf, (void*) info, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ztrttf_(void* transr, void* uplo, void* n, void* a, void* lda, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* transr, void* uplo, void* n, void* a, void* lda, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* transr, void* uplo, void* n, void* a, void* lda, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    *(void **) &fn      = current_backend->lapack.ztrttf.f77_blas_function;

    hook_pos_ztrttf ++;
    if( hook_pos_ztrttf < __flexiblas_hooks->ztrttf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ztrttf.f77_hook_function[hook_pos_ztrttf];
        fn_hook((void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) arf, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_ztrttf = 0;
        fn((void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) arf, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_ztrttf(void* transr, void* uplo, void* n, void* a, void* lda, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_ztrttf_")));
#else
void flexiblas_chain_ztrttf(void* transr, void* uplo, void* n, void* a, void* lda, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_ztrttf_((void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) arf, (void*) info, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



