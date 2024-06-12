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


static TLS_STORE uint8_t hook_pos_dlarrf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlarrf,DLARRF)(blasint* n, double* d, double* l, double* ld, blasint* clstrt, blasint* clend, double* w, double* wgap, double* werr, double* spdiam, double* clgapl, double* clgapr, double* pivmin, double* sigma, double* dplus, double* lplus, double* work, blasint* info)
#else
void FC_GLOBAL(dlarrf,DLARRF)(blasint* n, double* d, double* l, double* ld, blasint* clstrt, blasint* clend, double* w, double* wgap, double* werr, double* spdiam, double* clgapl, double* clgapr, double* pivmin, double* sigma, double* dplus, double* lplus, double* work, blasint* info)
#endif
{
    void (*fn) (void* n, void* d, void* l, void* ld, void* clstrt, void* clend, void* w, void* wgap, void* werr, void* spdiam, void* clgapl, void* clgapr, void* pivmin, void* sigma, void* dplus, void* lplus, void* work, void* info);
    void (*fn_hook) (void* n, void* d, void* l, void* ld, void* clstrt, void* clend, void* w, void* wgap, void* werr, void* spdiam, void* clgapl, void* clgapr, void* pivmin, void* sigma, void* dplus, void* lplus, void* work, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlarrf.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlarrf.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) d, (void*) l, (void*) ld, (void*) clstrt, (void*) clend, (void*) w, (void*) wgap, (void*) werr, (void*) spdiam, (void*) clgapl, (void*) clgapr, (void*) pivmin, (void*) sigma, (void*) dplus, (void*) lplus, (void*) work, (void*) info);
        return;
    } else {
        hook_pos_dlarrf = 0;
        fn_hook((void*) n, (void*) d, (void*) l, (void*) ld, (void*) clstrt, (void*) clend, (void*) w, (void*) wgap, (void*) werr, (void*) spdiam, (void*) clgapl, (void*) clgapr, (void*) pivmin, (void*) sigma, (void*) dplus, (void*) lplus, (void*) work, (void*) info);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dlarrf_(blasint* n, double* d, double* l, double* ld, blasint* clstrt, blasint* clend, double* w, double* wgap, double* werr, double* spdiam, double* clgapl, double* clgapr, double* pivmin, double* sigma, double* dplus, double* lplus, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlarrf,DLARRF)))));
#else
#ifndef __APPLE__
void dlarrf(blasint* n, double* d, double* l, double* ld, blasint* clstrt, blasint* clend, double* w, double* wgap, double* werr, double* spdiam, double* clgapl, double* clgapr, double* pivmin, double* sigma, double* dplus, double* lplus, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlarrf,DLARRF)))));
#else
void dlarrf(blasint* n, double* d, double* l, double* ld, blasint* clstrt, blasint* clend, double* w, double* wgap, double* werr, double* spdiam, double* clgapl, double* clgapr, double* pivmin, double* sigma, double* dplus, double* lplus, double* work, blasint* info){ FC_GLOBAL(dlarrf,DLARRF)((void*) n, (void*) d, (void*) l, (void*) ld, (void*) clstrt, (void*) clend, (void*) w, (void*) wgap, (void*) werr, (void*) spdiam, (void*) clgapl, (void*) clgapr, (void*) pivmin, (void*) sigma, (void*) dplus, (void*) lplus, (void*) work, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlarrf_(void* n, void* d, void* l, void* ld, void* clstrt, void* clend, void* w, void* wgap, void* werr, void* spdiam, void* clgapl, void* clgapr, void* pivmin, void* sigma, void* dplus, void* lplus, void* work, void* info)
{
    void (*fn) (void* n, void* d, void* l, void* ld, void* clstrt, void* clend, void* w, void* wgap, void* werr, void* spdiam, void* clgapl, void* clgapr, void* pivmin, void* sigma, void* dplus, void* lplus, void* work, void* info);

    *(void **) & fn = current_backend->lapack.dlarrf.f77_blas_function;

    fn((void*) n, (void*) d, (void*) l, (void*) ld, (void*) clstrt, (void*) clend, (void*) w, (void*) wgap, (void*) werr, (void*) spdiam, (void*) clgapl, (void*) clgapr, (void*) pivmin, (void*) sigma, (void*) dplus, (void*) lplus, (void*) work, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlarrf(void* n, void* d, void* l, void* ld, void* clstrt, void* clend, void* w, void* wgap, void* werr, void* spdiam, void* clgapl, void* clgapr, void* pivmin, void* sigma, void* dplus, void* lplus, void* work, void* info) __attribute__((alias("flexiblas_real_dlarrf_")));
#else
void flexiblas_real_dlarrf(void* n, void* d, void* l, void* ld, void* clstrt, void* clend, void* w, void* wgap, void* werr, void* spdiam, void* clgapl, void* clgapr, void* pivmin, void* sigma, void* dplus, void* lplus, void* work, void* info){flexiblas_real_dlarrf_((void*) n, (void*) d, (void*) l, (void*) ld, (void*) clstrt, (void*) clend, (void*) w, (void*) wgap, (void*) werr, (void*) spdiam, (void*) clgapl, (void*) clgapr, (void*) pivmin, (void*) sigma, (void*) dplus, (void*) lplus, (void*) work, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlarrf_(void* n, void* d, void* l, void* ld, void* clstrt, void* clend, void* w, void* wgap, void* werr, void* spdiam, void* clgapl, void* clgapr, void* pivmin, void* sigma, void* dplus, void* lplus, void* work, void* info)
{
    void (*fn) (void* n, void* d, void* l, void* ld, void* clstrt, void* clend, void* w, void* wgap, void* werr, void* spdiam, void* clgapl, void* clgapr, void* pivmin, void* sigma, void* dplus, void* lplus, void* work, void* info);
    void (*fn_hook) (void* n, void* d, void* l, void* ld, void* clstrt, void* clend, void* w, void* wgap, void* werr, void* spdiam, void* clgapl, void* clgapr, void* pivmin, void* sigma, void* dplus, void* lplus, void* work, void* info);

    *(void **) &fn      = current_backend->lapack.dlarrf.f77_blas_function;

    hook_pos_dlarrf ++;
    if( hook_pos_dlarrf < __flexiblas_hooks->dlarrf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlarrf.f77_hook_function[hook_pos_dlarrf];
        fn_hook((void*) n, (void*) d, (void*) l, (void*) ld, (void*) clstrt, (void*) clend, (void*) w, (void*) wgap, (void*) werr, (void*) spdiam, (void*) clgapl, (void*) clgapr, (void*) pivmin, (void*) sigma, (void*) dplus, (void*) lplus, (void*) work, (void*) info);
    } else {
        hook_pos_dlarrf = 0;
        fn((void*) n, (void*) d, (void*) l, (void*) ld, (void*) clstrt, (void*) clend, (void*) w, (void*) wgap, (void*) werr, (void*) spdiam, (void*) clgapl, (void*) clgapr, (void*) pivmin, (void*) sigma, (void*) dplus, (void*) lplus, (void*) work, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlarrf(void* n, void* d, void* l, void* ld, void* clstrt, void* clend, void* w, void* wgap, void* werr, void* spdiam, void* clgapl, void* clgapr, void* pivmin, void* sigma, void* dplus, void* lplus, void* work, void* info) __attribute__((alias("flexiblas_chain_dlarrf_")));
#else
void flexiblas_chain_dlarrf(void* n, void* d, void* l, void* ld, void* clstrt, void* clend, void* w, void* wgap, void* werr, void* spdiam, void* clgapl, void* clgapr, void* pivmin, void* sigma, void* dplus, void* lplus, void* work, void* info){flexiblas_chain_dlarrf_((void*) n, (void*) d, (void*) l, (void*) ld, (void*) clstrt, (void*) clend, (void*) w, (void*) wgap, (void*) werr, (void*) spdiam, (void*) clgapl, (void*) clgapr, (void*) pivmin, (void*) sigma, (void*) dplus, (void*) lplus, (void*) work, (void*) info);}
#endif



