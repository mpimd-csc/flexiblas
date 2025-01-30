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


static TLS_STORE uint8_t hook_pos_dlacn2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlacn2,DLACN2)(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase, blasint* isave)
#else
void FC_GLOBAL(dlacn2,DLACN2)(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase, blasint* isave)
#endif
{
    void (*fn) (void* n, void* v, void* x, void* isgn, void* est, void* kase, void* isave);
    void (*fn_hook) (void* n, void* v, void* x, void* isgn, void* est, void* kase, void* isave);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlacn2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlacn2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase, (void*) isave);
        return;
    } else {
        hook_pos_dlacn2 = 0;
        fn_hook((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase, (void*) isave);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlacn2,DLACN2)(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase, blasint* isave) __attribute__((alias(MTS(FC_GLOBAL(dlacn2,DLACN2)))));
void FC_GLOBAL3(dlacn2,DLACN2)(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase, blasint* isave) __attribute__((alias(MTS(FC_GLOBAL(dlacn2,DLACN2)))));
#else
void FC_GLOBAL2(dlacn2,DLACN2)(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase, blasint* isave){ FC_GLOBAL(dlacn2,DLACN2)((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase, (void*) isave); }
void FC_GLOBAL3(dlacn2,DLACN2)(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase, blasint* isave){ FC_GLOBAL(dlacn2,DLACN2)((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase, (void*) isave); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlacn2_(void* n, void* v, void* x, void* isgn, void* est, void* kase, void* isave)
{
    void (*fn) (void* n, void* v, void* x, void* isgn, void* est, void* kase, void* isave);

    *(void **) & fn = current_backend->lapack.dlacn2.f77_blas_function;

    fn((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase, (void*) isave);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlacn2(void* n, void* v, void* x, void* isgn, void* est, void* kase, void* isave) __attribute__((alias("flexiblas_real_dlacn2_")));
#else
void flexiblas_real_dlacn2(void* n, void* v, void* x, void* isgn, void* est, void* kase, void* isave){flexiblas_real_dlacn2_((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase, (void*) isave);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlacn2_(void* n, void* v, void* x, void* isgn, void* est, void* kase, void* isave)
{
    void (*fn) (void* n, void* v, void* x, void* isgn, void* est, void* kase, void* isave);
    void (*fn_hook) (void* n, void* v, void* x, void* isgn, void* est, void* kase, void* isave);

    *(void **) &fn      = current_backend->lapack.dlacn2.f77_blas_function;

    hook_pos_dlacn2 ++;
    if( hook_pos_dlacn2 < __flexiblas_hooks->dlacn2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlacn2.f77_hook_function[hook_pos_dlacn2];
        fn_hook((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase, (void*) isave);
    } else {
        hook_pos_dlacn2 = 0;
        fn((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase, (void*) isave);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlacn2(void* n, void* v, void* x, void* isgn, void* est, void* kase, void* isave) __attribute__((alias("flexiblas_chain_dlacn2_")));
#else
void flexiblas_chain_dlacn2(void* n, void* v, void* x, void* isgn, void* est, void* kase, void* isave){flexiblas_chain_dlacn2_((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase, (void*) isave);}
#endif



