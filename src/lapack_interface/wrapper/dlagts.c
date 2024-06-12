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


static TLS_STORE uint8_t hook_pos_dlagts = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlagts,DLAGTS)(blasint* job, blasint* n, double* a, double* b, double* c, double* d, blasint* in, double* y, double* tol, blasint* info)
#else
void FC_GLOBAL(dlagts,DLAGTS)(blasint* job, blasint* n, double* a, double* b, double* c, double* d, blasint* in, double* y, double* tol, blasint* info)
#endif
{
    void (*fn) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);
    void (*fn_hook) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlagts.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlagts.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);
        return;
    } else {
        hook_pos_dlagts = 0;
        fn_hook((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dlagts_(blasint* job, blasint* n, double* a, double* b, double* c, double* d, blasint* in, double* y, double* tol, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlagts,DLAGTS)))));
#else
#ifndef __APPLE__
void dlagts(blasint* job, blasint* n, double* a, double* b, double* c, double* d, blasint* in, double* y, double* tol, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlagts,DLAGTS)))));
#else
void dlagts(blasint* job, blasint* n, double* a, double* b, double* c, double* d, blasint* in, double* y, double* tol, blasint* info){ FC_GLOBAL(dlagts,DLAGTS)((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlagts_(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info)
{
    void (*fn) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);

    *(void **) & fn = current_backend->lapack.dlagts.f77_blas_function;

    fn((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlagts(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info) __attribute__((alias("flexiblas_real_dlagts_")));
#else
void flexiblas_real_dlagts(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info){flexiblas_real_dlagts_((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlagts_(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info)
{
    void (*fn) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);
    void (*fn_hook) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);

    *(void **) &fn      = current_backend->lapack.dlagts.f77_blas_function;

    hook_pos_dlagts ++;
    if( hook_pos_dlagts < __flexiblas_hooks->dlagts.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlagts.f77_hook_function[hook_pos_dlagts];
        fn_hook((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);
    } else {
        hook_pos_dlagts = 0;
        fn((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlagts(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info) __attribute__((alias("flexiblas_chain_dlagts_")));
#else
void flexiblas_chain_dlagts(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info){flexiblas_chain_dlagts_((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);}
#endif



