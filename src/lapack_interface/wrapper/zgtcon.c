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


static TLS_STORE uint8_t hook_pos_zgtcon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgtcon,ZGTCON)(char* norm, blasint* n, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, double* anorm, double* rcond, double complex* work, blasint* info, flexiblas_fortran_charlen_t len_norm)
#else
void FC_GLOBAL(zgtcon,ZGTCON)(char* norm, blasint* n, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, double* anorm, double* rcond, double complex* work, blasint* info, flexiblas_fortran_charlen_t len_norm)
#endif
{
    void (*fn) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm);
    void (*fn_hook) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zgtcon.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zgtcon.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
        return;
    } else {
        hook_pos_zgtcon = 0;
        fn_hook((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zgtcon,ZGTCON)(char* norm, blasint* n, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, double* anorm, double* rcond, double complex* work, blasint* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(zgtcon,ZGTCON)))));
void FC_GLOBAL3(zgtcon,ZGTCON)(char* norm, blasint* n, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, double* anorm, double* rcond, double complex* work, blasint* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(zgtcon,ZGTCON)))));
#else
void FC_GLOBAL2(zgtcon,ZGTCON)(char* norm, blasint* n, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, double* anorm, double* rcond, double complex* work, blasint* info, flexiblas_fortran_charlen_t len_norm){ FC_GLOBAL(zgtcon,ZGTCON)((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_norm); }
void FC_GLOBAL3(zgtcon,ZGTCON)(char* norm, blasint* n, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, double* anorm, double* rcond, double complex* work, blasint* info, flexiblas_fortran_charlen_t len_norm){ FC_GLOBAL(zgtcon,ZGTCON)((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_norm); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgtcon_(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm)
{
    void (*fn) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm);

    *(void **) & fn = current_backend->lapack.zgtcon.f77_blas_function;

    fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zgtcon(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_real_zgtcon_")));
#else
void flexiblas_real_zgtcon(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm){flexiblas_real_zgtcon_((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_norm);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgtcon_(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm)
{
    void (*fn) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm);
    void (*fn_hook) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm);

    *(void **) &fn      = current_backend->lapack.zgtcon.f77_blas_function;

    hook_pos_zgtcon ++;
    if( hook_pos_zgtcon < __flexiblas_hooks->zgtcon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgtcon.f77_hook_function[hook_pos_zgtcon];
        fn_hook((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
    } else {
        hook_pos_zgtcon = 0;
        fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zgtcon(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_chain_zgtcon_")));
#else
void flexiblas_chain_zgtcon(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info, flexiblas_fortran_charlen_t len_norm){flexiblas_chain_zgtcon_((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_norm);}
#endif



