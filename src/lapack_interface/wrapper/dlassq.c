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


static TLS_STORE uint8_t hook_pos_dlassq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlassq,DLASSQ)(blasint* n, float* x, blasint* incx, float* scale, float* sumsq)
#else
void FC_GLOBAL(dlassq,DLASSQ)(blasint* n, float* x, blasint* incx, float* scale, float* sumsq)
#endif
{
    void (*fn) (void* n, void* x, void* incx, void* scale, void* sumsq);
    void (*fn_hook) (void* n, void* x, void* incx, void* scale, void* sumsq);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlassq.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlassq.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq);
        return;
    } else {
        hook_pos_dlassq = 0;
        fn_hook((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlassq,DLASSQ)(blasint* n, float* x, blasint* incx, float* scale, float* sumsq) __attribute__((alias(MTS(FC_GLOBAL(dlassq,DLASSQ)))));
void FC_GLOBAL3(dlassq,DLASSQ)(blasint* n, float* x, blasint* incx, float* scale, float* sumsq) __attribute__((alias(MTS(FC_GLOBAL(dlassq,DLASSQ)))));
#else
void FC_GLOBAL2(dlassq,DLASSQ)(blasint* n, float* x, blasint* incx, float* scale, float* sumsq){ FC_GLOBAL(dlassq,DLASSQ)((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq); }
void FC_GLOBAL3(dlassq,DLASSQ)(blasint* n, float* x, blasint* incx, float* scale, float* sumsq){ FC_GLOBAL(dlassq,DLASSQ)((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlassq_(void* n, void* x, void* incx, void* scale, void* sumsq)
{
    void (*fn) (void* n, void* x, void* incx, void* scale, void* sumsq);

    *(void **) & fn = current_backend->lapack.dlassq.f77_blas_function;

    fn((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlassq(void* n, void* x, void* incx, void* scale, void* sumsq) __attribute__((alias("flexiblas_real_dlassq_")));
#else
void flexiblas_real_dlassq(void* n, void* x, void* incx, void* scale, void* sumsq){flexiblas_real_dlassq_((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlassq_(void* n, void* x, void* incx, void* scale, void* sumsq)
{
    void (*fn) (void* n, void* x, void* incx, void* scale, void* sumsq);
    void (*fn_hook) (void* n, void* x, void* incx, void* scale, void* sumsq);

    *(void **) &fn      = current_backend->lapack.dlassq.f77_blas_function;

    hook_pos_dlassq ++;
    if( hook_pos_dlassq < __flexiblas_hooks->dlassq.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlassq.f77_hook_function[hook_pos_dlassq];
        fn_hook((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq);
    } else {
        hook_pos_dlassq = 0;
        fn((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlassq(void* n, void* x, void* incx, void* scale, void* sumsq) __attribute__((alias("flexiblas_chain_dlassq_")));
#else
void flexiblas_chain_dlassq(void* n, void* x, void* incx, void* scale, void* sumsq){flexiblas_chain_dlassq_((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq);}
#endif



