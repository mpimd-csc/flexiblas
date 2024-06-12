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


static TLS_STORE uint8_t hook_pos_zlargv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlargv,ZLARGV)(blasint* n, double complex* x, blasint* incx, double complex* y, blasint* incy, double* c, blasint* incc)
#else
void FC_GLOBAL(zlargv,ZLARGV)(blasint* n, double complex* x, blasint* incx, double complex* y, blasint* incy, double* c, blasint* incc)
#endif
{
    void (*fn) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* incc);
    void (*fn_hook) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* incc);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlargv.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlargv.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) incc);
        return;
    } else {
        hook_pos_zlargv = 0;
        fn_hook((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) incc);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void zlargv_(blasint* n, double complex* x, blasint* incx, double complex* y, blasint* incy, double* c, blasint* incc) __attribute__((alias(MTS(FC_GLOBAL(zlargv,ZLARGV)))));
#else
#ifndef __APPLE__
void zlargv(blasint* n, double complex* x, blasint* incx, double complex* y, blasint* incy, double* c, blasint* incc) __attribute__((alias(MTS(FC_GLOBAL(zlargv,ZLARGV)))));
#else
void zlargv(blasint* n, double complex* x, blasint* incx, double complex* y, blasint* incy, double* c, blasint* incc){ FC_GLOBAL(zlargv,ZLARGV)((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) incc); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlargv_(void* n, void* x, void* incx, void* y, void* incy, void* c, void* incc)
{
    void (*fn) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* incc);

    *(void **) & fn = current_backend->lapack.zlargv.f77_blas_function;

    fn((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) incc);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlargv(void* n, void* x, void* incx, void* y, void* incy, void* c, void* incc) __attribute__((alias("flexiblas_real_zlargv_")));
#else
void flexiblas_real_zlargv(void* n, void* x, void* incx, void* y, void* incy, void* c, void* incc){flexiblas_real_zlargv_((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) incc);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlargv_(void* n, void* x, void* incx, void* y, void* incy, void* c, void* incc)
{
    void (*fn) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* incc);
    void (*fn_hook) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* incc);

    *(void **) &fn      = current_backend->lapack.zlargv.f77_blas_function;

    hook_pos_zlargv ++;
    if( hook_pos_zlargv < __flexiblas_hooks->zlargv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlargv.f77_hook_function[hook_pos_zlargv];
        fn_hook((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) incc);
    } else {
        hook_pos_zlargv = 0;
        fn((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) incc);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlargv(void* n, void* x, void* incx, void* y, void* incy, void* c, void* incc) __attribute__((alias("flexiblas_chain_zlargv_")));
#else
void flexiblas_chain_zlargv(void* n, void* x, void* incx, void* y, void* incy, void* c, void* incc){flexiblas_chain_zlargv_((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) incc);}
#endif



