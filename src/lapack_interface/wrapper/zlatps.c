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


static TLS_STORE uint8_t hook_pos_zlatps = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlatps,ZLATPS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, double complex* ap, double complex* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin)
#else
void FC_GLOBAL(zlatps,ZLATPS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, double complex* ap, double complex* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin)
#endif
{
    void (*fn) (void* uplo, void* trans, void* diag, void* normin, void* n, void* ap, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);
    void (*fn_hook) (void* uplo, void* trans, void* diag, void* normin, void* n, void* ap, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlatps.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlatps.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) ap, (void*) x, (void*) scale, (void*) cnorm, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag, ( flexiblas_fortran_charlen_t ) len_normin);
        return;
    } else {
        hook_pos_zlatps = 0;
        fn_hook((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) ap, (void*) x, (void*) scale, (void*) cnorm, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag, ( flexiblas_fortran_charlen_t ) len_normin);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zlatps,ZLATPS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, double complex* ap, double complex* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin) __attribute__((alias(MTS(FC_GLOBAL(zlatps,ZLATPS)))));
void FC_GLOBAL3(zlatps,ZLATPS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, double complex* ap, double complex* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin) __attribute__((alias(MTS(FC_GLOBAL(zlatps,ZLATPS)))));
#else
void FC_GLOBAL2(zlatps,ZLATPS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, double complex* ap, double complex* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin){ FC_GLOBAL(zlatps,ZLATPS)((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) ap, (void*) x, (void*) scale, (void*) cnorm, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag, (flexiblas_fortran_charlen_t) len_normin); }
void FC_GLOBAL3(zlatps,ZLATPS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, double complex* ap, double complex* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin){ FC_GLOBAL(zlatps,ZLATPS)((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) ap, (void*) x, (void*) scale, (void*) cnorm, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag, (flexiblas_fortran_charlen_t) len_normin); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlatps_(void* uplo, void* trans, void* diag, void* normin, void* n, void* ap, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin)
{
    void (*fn) (void* uplo, void* trans, void* diag, void* normin, void* n, void* ap, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    *(void **) & fn = current_backend->lapack.zlatps.f77_blas_function;

    fn((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) ap, (void*) x, (void*) scale, (void*) cnorm, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag, ( flexiblas_fortran_charlen_t ) len_normin);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlatps(void* uplo, void* trans, void* diag, void* normin, void* n, void* ap, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin) __attribute__((alias("flexiblas_real_zlatps_")));
#else
void flexiblas_real_zlatps(void* uplo, void* trans, void* diag, void* normin, void* n, void* ap, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin){flexiblas_real_zlatps_((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) ap, (void*) x, (void*) scale, (void*) cnorm, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag, (flexiblas_fortran_charlen_t) len_normin);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlatps_(void* uplo, void* trans, void* diag, void* normin, void* n, void* ap, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin)
{
    void (*fn) (void* uplo, void* trans, void* diag, void* normin, void* n, void* ap, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);
    void (*fn_hook) (void* uplo, void* trans, void* diag, void* normin, void* n, void* ap, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    *(void **) &fn      = current_backend->lapack.zlatps.f77_blas_function;

    hook_pos_zlatps ++;
    if( hook_pos_zlatps < __flexiblas_hooks->zlatps.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlatps.f77_hook_function[hook_pos_zlatps];
        fn_hook((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) ap, (void*) x, (void*) scale, (void*) cnorm, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag, ( flexiblas_fortran_charlen_t ) len_normin);
    } else {
        hook_pos_zlatps = 0;
        fn((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) ap, (void*) x, (void*) scale, (void*) cnorm, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag, ( flexiblas_fortran_charlen_t ) len_normin);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlatps(void* uplo, void* trans, void* diag, void* normin, void* n, void* ap, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin) __attribute__((alias("flexiblas_chain_zlatps_")));
#else
void flexiblas_chain_zlatps(void* uplo, void* trans, void* diag, void* normin, void* n, void* ap, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin){flexiblas_chain_zlatps_((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) ap, (void*) x, (void*) scale, (void*) cnorm, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag, (flexiblas_fortran_charlen_t) len_normin);}
#endif



