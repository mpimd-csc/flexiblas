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


static TLS_STORE uint8_t hook_pos_slasq5 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasq5,SLASQ5)(blasint* i0, blasint* n0, float* z, blasint* pp, float* tau, float* sigma, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2, blaslogical* ieee, float* eps)
#else
void FC_GLOBAL(slasq5,SLASQ5)(blasint* i0, blasint* n0, float* z, blasint* pp, float* tau, float* sigma, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2, blaslogical* ieee, float* eps)
#endif
{
    void (*fn) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);
    void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slasq5.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slasq5.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);
        return;
    } else {
        hook_pos_slasq5 = 0;
        fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slasq5,SLASQ5)(blasint* i0, blasint* n0, float* z, blasint* pp, float* tau, float* sigma, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2, blaslogical* ieee, float* eps) __attribute__((alias(MTS(FC_GLOBAL(slasq5,SLASQ5)))));
void FC_GLOBAL3(slasq5,SLASQ5)(blasint* i0, blasint* n0, float* z, blasint* pp, float* tau, float* sigma, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2, blaslogical* ieee, float* eps) __attribute__((alias(MTS(FC_GLOBAL(slasq5,SLASQ5)))));
#else
void FC_GLOBAL2(slasq5,SLASQ5)(blasint* i0, blasint* n0, float* z, blasint* pp, float* tau, float* sigma, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2, blaslogical* ieee, float* eps){ FC_GLOBAL(slasq5,SLASQ5)((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps); }
void FC_GLOBAL3(slasq5,SLASQ5)(blasint* i0, blasint* n0, float* z, blasint* pp, float* tau, float* sigma, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2, blaslogical* ieee, float* eps){ FC_GLOBAL(slasq5,SLASQ5)((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasq5_(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps)
{
    void (*fn) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);

    *(void **) & fn = current_backend->lapack.slasq5.f77_blas_function;

    fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slasq5(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps) __attribute__((alias("flexiblas_real_slasq5_")));
#else
void flexiblas_real_slasq5(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps){flexiblas_real_slasq5_((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slasq5_(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps)
{
    void (*fn) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);
    void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);

    *(void **) &fn      = current_backend->lapack.slasq5.f77_blas_function;

    hook_pos_slasq5 ++;
    if( hook_pos_slasq5 < __flexiblas_hooks->slasq5.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slasq5.f77_hook_function[hook_pos_slasq5];
        fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);
    } else {
        hook_pos_slasq5 = 0;
        fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slasq5(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps) __attribute__((alias("flexiblas_chain_slasq5_")));
#else
void flexiblas_chain_slasq5(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps){flexiblas_chain_slasq5_((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);}
#endif



