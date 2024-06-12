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


static TLS_STORE uint8_t hook_pos_dptrfs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dptrfs,DPTRFS)(blasint* n, blasint* nrhs, double* d, double* e, double* df, double* ef, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* info)
#else
void FC_GLOBAL(dptrfs,DPTRFS)(blasint* n, blasint* nrhs, double* d, double* e, double* df, double* ef, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* info)
#endif
{
    void (*fn) (void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* info);
    void (*fn_hook) (void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dptrfs.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dptrfs.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) info);
        return;
    } else {
        hook_pos_dptrfs = 0;
        fn_hook((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) info);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dptrfs_(blasint* n, blasint* nrhs, double* d, double* e, double* df, double* ef, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dptrfs,DPTRFS)))));
#else
#ifndef __APPLE__
void dptrfs(blasint* n, blasint* nrhs, double* d, double* e, double* df, double* ef, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dptrfs,DPTRFS)))));
#else
void dptrfs(blasint* n, blasint* nrhs, double* d, double* e, double* df, double* ef, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* info){ FC_GLOBAL(dptrfs,DPTRFS)((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dptrfs_(void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* info)
{
    void (*fn) (void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* info);

    *(void **) & fn = current_backend->lapack.dptrfs.f77_blas_function;

    fn((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dptrfs(void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* info) __attribute__((alias("flexiblas_real_dptrfs_")));
#else
void flexiblas_real_dptrfs(void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* info){flexiblas_real_dptrfs_((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dptrfs_(void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* info)
{
    void (*fn) (void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* info);
    void (*fn_hook) (void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* info);

    *(void **) &fn      = current_backend->lapack.dptrfs.f77_blas_function;

    hook_pos_dptrfs ++;
    if( hook_pos_dptrfs < __flexiblas_hooks->dptrfs.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dptrfs.f77_hook_function[hook_pos_dptrfs];
        fn_hook((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) info);
    } else {
        hook_pos_dptrfs = 0;
        fn((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dptrfs(void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* info) __attribute__((alias("flexiblas_chain_dptrfs_")));
#else
void flexiblas_chain_dptrfs(void* n, void* nrhs, void* d, void* e, void* df, void* ef, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* info){flexiblas_chain_dptrfs_((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) df, (void*) ef, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) info);}
#endif



