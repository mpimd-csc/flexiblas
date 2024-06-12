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


static TLS_STORE uint8_t hook_pos_cgtcon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgtcon,CGTCON)(char* norm, blasint* n, float complex* dl, float complex* d, float complex* du, float complex* du2, blasint* ipiv, float* anorm, float* rcond, float complex* work, blasint* info, flexiblas_fortran_charlen_t len_norm)
#else
void FC_GLOBAL(cgtcon,CGTCON)(char* norm, blasint* n, float complex* dl, float complex* d, float complex* du, float complex* du2, blasint* ipiv, float* anorm, float* rcond, float complex* work, blasint* info, flexiblas_fortran_charlen_t len_norm)
#endif
{
    void (*fn) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm);
    void (*fn_hook) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.cgtcon.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->cgtcon.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
        return;
    } else {
        hook_pos_cgtcon = 0;
        fn_hook((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void cgtcon_(char* norm, blasint* n, float complex* dl, float complex* d, float complex* du, float complex* du2, blasint* ipiv, float* anorm, float* rcond, float complex* work, blasint* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(cgtcon,CGTCON)))));
#else
#ifndef __APPLE__
void cgtcon(char* norm, blasint* n, float complex* dl, float complex* d, float complex* du, float complex* du2, blasint* ipiv, float* anorm, float* rcond, float complex* work, blasint* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(cgtcon,CGTCON)))));
#else
void cgtcon(char* norm, blasint* n, float complex* dl, float complex* d, float complex* du, float complex* du2, blasint* ipiv, float* anorm, float* rcond, float complex* work, blasint* info, flexiblas_fortran_charlen_t len_norm){ FC_GLOBAL(cgtcon,CGTCON)((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_norm); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgtcon_(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm)
{
    void (*fn) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm);

    *(void **) & fn = current_backend->lapack.cgtcon.f77_blas_function;

    fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);

    return;
}
#ifndef __APPLE__
void flexiblas_real_cgtcon(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_real_cgtcon_")));
#else
void flexiblas_real_cgtcon(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm){flexiblas_real_cgtcon_((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_norm);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgtcon_(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm)
{
    void (*fn) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm);
    void (*fn_hook) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm);

    *(void **) &fn      = current_backend->lapack.cgtcon.f77_blas_function;

    hook_pos_cgtcon ++;
    if( hook_pos_cgtcon < __flexiblas_hooks->cgtcon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgtcon.f77_hook_function[hook_pos_cgtcon];
        fn_hook((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
    } else {
        hook_pos_cgtcon = 0;
        fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_cgtcon(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_chain_cgtcon_")));
#else
void flexiblas_chain_cgtcon(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm){flexiblas_chain_cgtcon_((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_norm);}
#endif



