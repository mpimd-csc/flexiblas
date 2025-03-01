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


static TLS_STORE uint8_t hook_pos_zlagtm = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlagtm,ZLAGTM)(char* trans, blasint* n, blasint* nrhs, double* alpha, double complex* dl, double complex* d, double complex* du, double complex* x, blasint* ldx, double* beta, double complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans)
#else
void FC_GLOBAL(zlagtm,ZLAGTM)(char* trans, blasint* n, blasint* nrhs, double* alpha, double complex* dl, double complex* d, double complex* du, double complex* x, blasint* ldx, double* beta, double complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans)
#endif
{
    void (*fn) (void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans);
    void (*fn_hook) (void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlagtm.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlagtm.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_trans);
        return;
    } else {
        hook_pos_zlagtm = 0;
        fn_hook((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_trans);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zlagtm,ZLAGTM)(char* trans, blasint* n, blasint* nrhs, double* alpha, double complex* dl, double complex* d, double complex* du, double complex* x, blasint* ldx, double* beta, double complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(zlagtm,ZLAGTM)))));
void FC_GLOBAL3(zlagtm,ZLAGTM)(char* trans, blasint* n, blasint* nrhs, double* alpha, double complex* dl, double complex* d, double complex* du, double complex* x, blasint* ldx, double* beta, double complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(zlagtm,ZLAGTM)))));
#else
void FC_GLOBAL2(zlagtm,ZLAGTM)(char* trans, blasint* n, blasint* nrhs, double* alpha, double complex* dl, double complex* d, double complex* du, double complex* x, blasint* ldx, double* beta, double complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(zlagtm,ZLAGTM)((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, (flexiblas_fortran_charlen_t) len_trans); }
void FC_GLOBAL3(zlagtm,ZLAGTM)(char* trans, blasint* n, blasint* nrhs, double* alpha, double complex* dl, double complex* d, double complex* du, double complex* x, blasint* ldx, double* beta, double complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(zlagtm,ZLAGTM)((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, (flexiblas_fortran_charlen_t) len_trans); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlagtm_(void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans)
{
    void (*fn) (void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans);

    *(void **) & fn = current_backend->lapack.zlagtm.f77_blas_function;

    fn((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_trans);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlagtm(void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_zlagtm_")));
#else
void flexiblas_real_zlagtm(void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans){flexiblas_real_zlagtm_((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlagtm_(void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans)
{
    void (*fn) (void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans);
    void (*fn_hook) (void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans);

    *(void **) &fn      = current_backend->lapack.zlagtm.f77_blas_function;

    hook_pos_zlagtm ++;
    if( hook_pos_zlagtm < __flexiblas_hooks->zlagtm.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlagtm.f77_hook_function[hook_pos_zlagtm];
        fn_hook((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_trans);
    } else {
        hook_pos_zlagtm = 0;
        fn((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_trans);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlagtm(void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_zlagtm_")));
#else
void flexiblas_chain_zlagtm(void* trans, void* n, void* nrhs, void* alpha, void* dl, void* d, void* du, void* x, void* ldx, void* beta, void* b, void* ldb, flexiblas_fortran_charlen_t len_trans){flexiblas_chain_zlagtm_((void*) trans, (void*) n, (void*) nrhs, (void*) alpha, (void*) dl, (void*) d, (void*) du, (void*) x, (void*) ldx, (void*) beta, (void*) b, (void*) ldb, (flexiblas_fortran_charlen_t) len_trans);}
#endif



