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


static TLS_STORE uint8_t hook_pos_clagtm = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clagtm,CLAGTM)(char* trans, blasint* n, blasint* nrhs, float* alpha, float complex* dl, float complex* d, float complex* du, float complex* x, blasint* ldx, float* beta, float complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans)
#else
void FC_GLOBAL(clagtm,CLAGTM)(char* trans, blasint* n, blasint* nrhs, float* alpha, float complex* dl, float complex* d, float complex* du, float complex* x, blasint* ldx, float* beta, float complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans)
#endif
{
    void (*fn) (void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans);
    void (*fn_hook) (void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.clagtm.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->clagtm.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_trans);
        return;
    } else {
        hook_pos_clagtm = 0;
        fn_hook((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_trans);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(clagtm,CLAGTM)(char* trans, blasint* n, blasint* nrhs, float* alpha, float complex* dl, float complex* d, float complex* du, float complex* x, blasint* ldx, float* beta, float complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(clagtm,CLAGTM)))));
void FC_GLOBAL3(clagtm,CLAGTM)(char* trans, blasint* n, blasint* nrhs, float* alpha, float complex* dl, float complex* d, float complex* du, float complex* x, blasint* ldx, float* beta, float complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(clagtm,CLAGTM)))));
#else
void FC_GLOBAL2(clagtm,CLAGTM)(char* trans, blasint* n, blasint* nrhs, float* alpha, float complex* dl, float complex* d, float complex* du, float complex* x, blasint* ldx, float* beta, float complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(clagtm,CLAGTM)((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, (flexiblas_fortran_charlen_t) len_trans); }
void FC_GLOBAL3(clagtm,CLAGTM)(char* trans, blasint* n, blasint* nrhs, float* alpha, float complex* dl, float complex* d, float complex* du, float complex* x, blasint* ldx, float* beta, float complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(clagtm,CLAGTM)((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, (flexiblas_fortran_charlen_t) len_trans); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clagtm_(void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans)
{
    void (*fn) (void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans);

    *(void **) & fn = current_backend->lapack.clagtm.f77_blas_function;

    fn((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_trans);

    return;
}
#ifndef __APPLE__
void flexiblas_real_clagtm(void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_clagtm_")));
#else
void flexiblas_real_clagtm(void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans){flexiblas_real_clagtm_((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clagtm_(void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans)
{
    void (*fn) (void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans);
    void (*fn_hook) (void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans);

    *(void **) &fn      = current_backend->lapack.clagtm.f77_blas_function;

    hook_pos_clagtm ++;
    if( hook_pos_clagtm < __flexiblas_hooks->clagtm.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clagtm.f77_hook_function[hook_pos_clagtm];
        fn_hook((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_trans);
    } else {
        hook_pos_clagtm = 0;
        fn((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_trans);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_clagtm(void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_clagtm_")));
#else
void flexiblas_chain_clagtm(void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans){flexiblas_chain_clagtm_((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, (flexiblas_fortran_charlen_t) len_trans);}
#endif



