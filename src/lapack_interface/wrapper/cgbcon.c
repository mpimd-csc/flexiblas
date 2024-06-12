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


static TLS_STORE uint8_t hook_pos_cgbcon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgbcon,CGBCON)(char* norm, blasint* n, blasint* kl, blasint* ku, float complex* ab, blasint* ldab, blasint* ipiv, float* anorm, float* rcond, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm)
#else
void FC_GLOBAL(cgbcon,CGBCON)(char* norm, blasint* n, blasint* kl, blasint* ku, float complex* ab, blasint* ldab, blasint* ipiv, float* anorm, float* rcond, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm)
#endif
{
    void (*fn) (void* norm, void* n, void* kl, void* ku, void* ab, void* ldab, void* ipiv, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);
    void (*fn_hook) (void* norm, void* n, void* kl, void* ku, void* ab, void* ldab, void* ipiv, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.cgbcon.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->cgbcon.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) norm, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
        return;
    } else {
        hook_pos_cgbcon = 0;
        fn_hook((void*) norm, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void cgbcon_(char* norm, blasint* n, blasint* kl, blasint* ku, float complex* ab, blasint* ldab, blasint* ipiv, float* anorm, float* rcond, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(cgbcon,CGBCON)))));
#else
#ifndef __APPLE__
void cgbcon(char* norm, blasint* n, blasint* kl, blasint* ku, float complex* ab, blasint* ldab, blasint* ipiv, float* anorm, float* rcond, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(cgbcon,CGBCON)))));
#else
void cgbcon(char* norm, blasint* n, blasint* kl, blasint* ku, float complex* ab, blasint* ldab, blasint* ipiv, float* anorm, float* rcond, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm){ FC_GLOBAL(cgbcon,CGBCON)((void*) norm, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgbcon_(void* norm, void* n, void* kl, void* ku, void* ab, void* ldab, void* ipiv, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm)
{
    void (*fn) (void* norm, void* n, void* kl, void* ku, void* ab, void* ldab, void* ipiv, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);

    *(void **) & fn = current_backend->lapack.cgbcon.f77_blas_function;

    fn((void*) norm, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);

    return;
}
#ifndef __APPLE__
void flexiblas_real_cgbcon(void* norm, void* n, void* kl, void* ku, void* ab, void* ldab, void* ipiv, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_real_cgbcon_")));
#else
void flexiblas_real_cgbcon(void* norm, void* n, void* kl, void* ku, void* ab, void* ldab, void* ipiv, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm){flexiblas_real_cgbcon_((void*) norm, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgbcon_(void* norm, void* n, void* kl, void* ku, void* ab, void* ldab, void* ipiv, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm)
{
    void (*fn) (void* norm, void* n, void* kl, void* ku, void* ab, void* ldab, void* ipiv, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);
    void (*fn_hook) (void* norm, void* n, void* kl, void* ku, void* ab, void* ldab, void* ipiv, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);

    *(void **) &fn      = current_backend->lapack.cgbcon.f77_blas_function;

    hook_pos_cgbcon ++;
    if( hook_pos_cgbcon < __flexiblas_hooks->cgbcon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgbcon.f77_hook_function[hook_pos_cgbcon];
        fn_hook((void*) norm, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
    } else {
        hook_pos_cgbcon = 0;
        fn((void*) norm, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_cgbcon(void* norm, void* n, void* kl, void* ku, void* ab, void* ldab, void* ipiv, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_chain_cgbcon_")));
#else
void flexiblas_chain_cgbcon(void* norm, void* n, void* kl, void* ku, void* ab, void* ldab, void* ipiv, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm){flexiblas_chain_cgbcon_((void*) norm, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm);}
#endif



