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


static TLS_STORE uint8_t hook_pos_dlatbs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlatbs,DLATBS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, blasint* kd, double* ab, blasint* ldab, double* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin)
#else
void FC_GLOBAL(dlatbs,DLATBS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, blasint* kd, double* ab, blasint* ldab, double* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin)
#endif
{
    void (*fn) (void* uplo, void* trans, void* diag, void* normin, void* n, void* kd, void* ab, void* ldab, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);
    void (*fn_hook) (void* uplo, void* trans, void* diag, void* normin, void* n, void* kd, void* ab, void* ldab, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlatbs.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlatbs.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) x, (void*) scale, (void*) cnorm, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag, ( flexiblas_fortran_charlen_t ) len_normin);
        return;
    } else {
        hook_pos_dlatbs = 0;
        fn_hook((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) x, (void*) scale, (void*) cnorm, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag, ( flexiblas_fortran_charlen_t ) len_normin);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlatbs,DLATBS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, blasint* kd, double* ab, blasint* ldab, double* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin) __attribute__((alias(MTS(FC_GLOBAL(dlatbs,DLATBS)))));
void FC_GLOBAL3(dlatbs,DLATBS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, blasint* kd, double* ab, blasint* ldab, double* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin) __attribute__((alias(MTS(FC_GLOBAL(dlatbs,DLATBS)))));
#else
void FC_GLOBAL2(dlatbs,DLATBS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, blasint* kd, double* ab, blasint* ldab, double* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin){ FC_GLOBAL(dlatbs,DLATBS)((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) x, (void*) scale, (void*) cnorm, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag, (flexiblas_fortran_charlen_t) len_normin); }
void FC_GLOBAL3(dlatbs,DLATBS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, blasint* kd, double* ab, blasint* ldab, double* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin){ FC_GLOBAL(dlatbs,DLATBS)((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) x, (void*) scale, (void*) cnorm, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag, (flexiblas_fortran_charlen_t) len_normin); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlatbs_(void* uplo, void* trans, void* diag, void* normin, void* n, void* kd, void* ab, void* ldab, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin)
{
    void (*fn) (void* uplo, void* trans, void* diag, void* normin, void* n, void* kd, void* ab, void* ldab, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    *(void **) & fn = current_backend->lapack.dlatbs.f77_blas_function;

    fn((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) x, (void*) scale, (void*) cnorm, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag, ( flexiblas_fortran_charlen_t ) len_normin);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlatbs(void* uplo, void* trans, void* diag, void* normin, void* n, void* kd, void* ab, void* ldab, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin) __attribute__((alias("flexiblas_real_dlatbs_")));
#else
void flexiblas_real_dlatbs(void* uplo, void* trans, void* diag, void* normin, void* n, void* kd, void* ab, void* ldab, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin){flexiblas_real_dlatbs_((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) x, (void*) scale, (void*) cnorm, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag, (flexiblas_fortran_charlen_t) len_normin);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlatbs_(void* uplo, void* trans, void* diag, void* normin, void* n, void* kd, void* ab, void* ldab, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin)
{
    void (*fn) (void* uplo, void* trans, void* diag, void* normin, void* n, void* kd, void* ab, void* ldab, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);
    void (*fn_hook) (void* uplo, void* trans, void* diag, void* normin, void* n, void* kd, void* ab, void* ldab, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    *(void **) &fn      = current_backend->lapack.dlatbs.f77_blas_function;

    hook_pos_dlatbs ++;
    if( hook_pos_dlatbs < __flexiblas_hooks->dlatbs.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlatbs.f77_hook_function[hook_pos_dlatbs];
        fn_hook((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) x, (void*) scale, (void*) cnorm, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag, ( flexiblas_fortran_charlen_t ) len_normin);
    } else {
        hook_pos_dlatbs = 0;
        fn((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) x, (void*) scale, (void*) cnorm, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_diag, ( flexiblas_fortran_charlen_t ) len_normin);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlatbs(void* uplo, void* trans, void* diag, void* normin, void* n, void* kd, void* ab, void* ldab, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin) __attribute__((alias("flexiblas_chain_dlatbs_")));
#else
void flexiblas_chain_dlatbs(void* uplo, void* trans, void* diag, void* normin, void* n, void* kd, void* ab, void* ldab, void* x, void* scale, void* cnorm, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin){flexiblas_chain_dlatbs_((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) x, (void*) scale, (void*) cnorm, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_diag, (flexiblas_fortran_charlen_t) len_normin);}
#endif



