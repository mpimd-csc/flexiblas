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


static TLS_STORE uint8_t hook_pos_slagts = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slagts,SLAGTS)(blasint* job, blasint* n, float* a, float* b, float* c, float* d, blasint* in, float* y, float* tol, blasint* info)
#else
void FC_GLOBAL(slagts,SLAGTS)(blasint* job, blasint* n, float* a, float* b, float* c, float* d, blasint* in, float* y, float* tol, blasint* info)
#endif
{
    void (*fn) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);
    void (*fn_hook) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slagts.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slagts.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);
        return;
    } else {
        hook_pos_slagts = 0;
        fn_hook((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slagts,SLAGTS)(blasint* job, blasint* n, float* a, float* b, float* c, float* d, blasint* in, float* y, float* tol, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slagts,SLAGTS)))));
void FC_GLOBAL3(slagts,SLAGTS)(blasint* job, blasint* n, float* a, float* b, float* c, float* d, blasint* in, float* y, float* tol, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slagts,SLAGTS)))));
#else
void FC_GLOBAL2(slagts,SLAGTS)(blasint* job, blasint* n, float* a, float* b, float* c, float* d, blasint* in, float* y, float* tol, blasint* info){ FC_GLOBAL(slagts,SLAGTS)((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info); }
void FC_GLOBAL3(slagts,SLAGTS)(blasint* job, blasint* n, float* a, float* b, float* c, float* d, blasint* in, float* y, float* tol, blasint* info){ FC_GLOBAL(slagts,SLAGTS)((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slagts_(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info)
{
    void (*fn) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);

    *(void **) & fn = current_backend->lapack.slagts.f77_blas_function;

    fn((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slagts(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info) __attribute__((alias("flexiblas_real_slagts_")));
#else
void flexiblas_real_slagts(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info){flexiblas_real_slagts_((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slagts_(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info)
{
    void (*fn) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);
    void (*fn_hook) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);

    *(void **) &fn      = current_backend->lapack.slagts.f77_blas_function;

    hook_pos_slagts ++;
    if( hook_pos_slagts < __flexiblas_hooks->slagts.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slagts.f77_hook_function[hook_pos_slagts];
        fn_hook((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);
    } else {
        hook_pos_slagts = 0;
        fn((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slagts(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info) __attribute__((alias("flexiblas_chain_slagts_")));
#else
void flexiblas_chain_slagts(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info){flexiblas_chain_slagts_((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);}
#endif



