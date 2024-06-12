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


static TLS_STORE uint8_t hook_pos_slasd5 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasd5,SLASD5)(blasint* i, float* d, float* z, float* delta, float* rho, float* dsigma, float* work)
#else
void FC_GLOBAL(slasd5,SLASD5)(blasint* i, float* d, float* z, float* delta, float* rho, float* dsigma, float* work)
#endif
{
    void (*fn) (void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work);
    void (*fn_hook) (void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slasd5.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slasd5.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work);
        return;
    } else {
        hook_pos_slasd5 = 0;
        fn_hook((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void slasd5_(blasint* i, float* d, float* z, float* delta, float* rho, float* dsigma, float* work) __attribute__((alias(MTS(FC_GLOBAL(slasd5,SLASD5)))));
#else
#ifndef __APPLE__
void slasd5(blasint* i, float* d, float* z, float* delta, float* rho, float* dsigma, float* work) __attribute__((alias(MTS(FC_GLOBAL(slasd5,SLASD5)))));
#else
void slasd5(blasint* i, float* d, float* z, float* delta, float* rho, float* dsigma, float* work){ FC_GLOBAL(slasd5,SLASD5)((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasd5_(void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work)
{
    void (*fn) (void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work);

    *(void **) & fn = current_backend->lapack.slasd5.f77_blas_function;

    fn((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slasd5(void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work) __attribute__((alias("flexiblas_real_slasd5_")));
#else
void flexiblas_real_slasd5(void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work){flexiblas_real_slasd5_((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slasd5_(void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work)
{
    void (*fn) (void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work);
    void (*fn_hook) (void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work);

    *(void **) &fn      = current_backend->lapack.slasd5.f77_blas_function;

    hook_pos_slasd5 ++;
    if( hook_pos_slasd5 < __flexiblas_hooks->slasd5.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slasd5.f77_hook_function[hook_pos_slasd5];
        fn_hook((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work);
    } else {
        hook_pos_slasd5 = 0;
        fn((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slasd5(void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work) __attribute__((alias("flexiblas_chain_slasd5_")));
#else
void flexiblas_chain_slasd5(void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work){flexiblas_chain_slasd5_((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work);}
#endif



