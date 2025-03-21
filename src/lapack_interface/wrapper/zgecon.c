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


static TLS_STORE uint8_t hook_pos_zgecon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgecon,ZGECON)(char* norm, blasint* n, double complex* a, blasint* lda, double* anorm, double* rcond, double complex* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm)
#else
void FC_GLOBAL(zgecon,ZGECON)(char* norm, blasint* n, double complex* a, blasint* lda, double* anorm, double* rcond, double complex* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm)
#endif
{
    void (*fn) (void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);
    void (*fn_hook) (void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zgecon.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zgecon.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
        return;
    } else {
        hook_pos_zgecon = 0;
        fn_hook((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zgecon,ZGECON)(char* norm, blasint* n, double complex* a, blasint* lda, double* anorm, double* rcond, double complex* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(zgecon,ZGECON)))));
void FC_GLOBAL3(zgecon,ZGECON)(char* norm, blasint* n, double complex* a, blasint* lda, double* anorm, double* rcond, double complex* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(zgecon,ZGECON)))));
#else
void FC_GLOBAL2(zgecon,ZGECON)(char* norm, blasint* n, double complex* a, blasint* lda, double* anorm, double* rcond, double complex* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm){ FC_GLOBAL(zgecon,ZGECON)((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm); }
void FC_GLOBAL3(zgecon,ZGECON)(char* norm, blasint* n, double complex* a, blasint* lda, double* anorm, double* rcond, double complex* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm){ FC_GLOBAL(zgecon,ZGECON)((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgecon_(void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm)
{
    void (*fn) (void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);

    *(void **) & fn = current_backend->lapack.zgecon.f77_blas_function;

    fn((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zgecon(void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_real_zgecon_")));
#else
void flexiblas_real_zgecon(void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm){flexiblas_real_zgecon_((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgecon_(void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm)
{
    void (*fn) (void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);
    void (*fn_hook) (void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);

    *(void **) &fn      = current_backend->lapack.zgecon.f77_blas_function;

    hook_pos_zgecon ++;
    if( hook_pos_zgecon < __flexiblas_hooks->zgecon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgecon.f77_hook_function[hook_pos_zgecon];
        fn_hook((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
    } else {
        hook_pos_zgecon = 0;
        fn((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zgecon(void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_chain_zgecon_")));
#else
void flexiblas_chain_zgecon(void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm){flexiblas_chain_zgecon_((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm);}
#endif



