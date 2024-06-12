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


static TLS_STORE uint8_t hook_pos_cptcon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cptcon,CPTCON)(blasint* n, float* d, float complex* e, float* anorm, float* rcond, float* rwork, blasint* info)
#else
void FC_GLOBAL(cptcon,CPTCON)(blasint* n, float* d, float complex* e, float* anorm, float* rcond, float* rwork, blasint* info)
#endif
{
    void (*fn) (void* n, void* d, void* e, void* anorm, void* rcond, void* rwork, void* info);
    void (*fn_hook) (void* n, void* d, void* e, void* anorm, void* rcond, void* rwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.cptcon.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->cptcon.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) rwork, (void*) info);
        return;
    } else {
        hook_pos_cptcon = 0;
        fn_hook((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) rwork, (void*) info);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void cptcon_(blasint* n, float* d, float complex* e, float* anorm, float* rcond, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cptcon,CPTCON)))));
#else
#ifndef __APPLE__
void cptcon(blasint* n, float* d, float complex* e, float* anorm, float* rcond, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cptcon,CPTCON)))));
#else
void cptcon(blasint* n, float* d, float complex* e, float* anorm, float* rcond, float* rwork, blasint* info){ FC_GLOBAL(cptcon,CPTCON)((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) rwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cptcon_(void* n, void* d, void* e, void* anorm, void* rcond, void* rwork, void* info)
{
    void (*fn) (void* n, void* d, void* e, void* anorm, void* rcond, void* rwork, void* info);

    *(void **) & fn = current_backend->lapack.cptcon.f77_blas_function;

    fn((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) rwork, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_cptcon(void* n, void* d, void* e, void* anorm, void* rcond, void* rwork, void* info) __attribute__((alias("flexiblas_real_cptcon_")));
#else
void flexiblas_real_cptcon(void* n, void* d, void* e, void* anorm, void* rcond, void* rwork, void* info){flexiblas_real_cptcon_((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) rwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cptcon_(void* n, void* d, void* e, void* anorm, void* rcond, void* rwork, void* info)
{
    void (*fn) (void* n, void* d, void* e, void* anorm, void* rcond, void* rwork, void* info);
    void (*fn_hook) (void* n, void* d, void* e, void* anorm, void* rcond, void* rwork, void* info);

    *(void **) &fn      = current_backend->lapack.cptcon.f77_blas_function;

    hook_pos_cptcon ++;
    if( hook_pos_cptcon < __flexiblas_hooks->cptcon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cptcon.f77_hook_function[hook_pos_cptcon];
        fn_hook((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) rwork, (void*) info);
    } else {
        hook_pos_cptcon = 0;
        fn((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) rwork, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_cptcon(void* n, void* d, void* e, void* anorm, void* rcond, void* rwork, void* info) __attribute__((alias("flexiblas_chain_cptcon_")));
#else
void flexiblas_chain_cptcon(void* n, void* d, void* e, void* anorm, void* rcond, void* rwork, void* info){flexiblas_chain_cptcon_((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) rwork, (void*) info);}
#endif



