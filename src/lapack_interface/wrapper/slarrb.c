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


static TLS_STORE uint8_t hook_pos_slarrb = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slarrb,SLARRB)(blasint* n, float* d, float* lld, blasint* ifirst, blasint* ilast, float* rtol1, float* rtol2, blasint* offset, float* w, float* wgap, float* werr, float* work, blasint* iwork, float* pivmin, float* spdiam, blasint* twist, blasint* info)
#else
void FC_GLOBAL(slarrb,SLARRB)(blasint* n, float* d, float* lld, blasint* ifirst, blasint* ilast, float* rtol1, float* rtol2, blasint* offset, float* w, float* wgap, float* werr, float* work, blasint* iwork, float* pivmin, float* spdiam, blasint* twist, blasint* info)
#endif
{
    void (*fn) (void* n, void* d, void* lld, void* ifirst, void* ilast, void* rtol1, void* rtol2, void* offset, void* w, void* wgap, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* twist, void* info);
    void (*fn_hook) (void* n, void* d, void* lld, void* ifirst, void* ilast, void* rtol1, void* rtol2, void* offset, void* w, void* wgap, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* twist, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slarrb.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slarrb.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) d, (void*) lld, (void*) ifirst, (void*) ilast, (void*) rtol1, (void*) rtol2, (void*) offset, (void*) w, (void*) wgap, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) twist, (void*) info);
        return;
    } else {
        hook_pos_slarrb = 0;
        fn_hook((void*) n, (void*) d, (void*) lld, (void*) ifirst, (void*) ilast, (void*) rtol1, (void*) rtol2, (void*) offset, (void*) w, (void*) wgap, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) twist, (void*) info);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void slarrb_(blasint* n, float* d, float* lld, blasint* ifirst, blasint* ilast, float* rtol1, float* rtol2, blasint* offset, float* w, float* wgap, float* werr, float* work, blasint* iwork, float* pivmin, float* spdiam, blasint* twist, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slarrb,SLARRB)))));
#else
#ifndef __APPLE__
void slarrb(blasint* n, float* d, float* lld, blasint* ifirst, blasint* ilast, float* rtol1, float* rtol2, blasint* offset, float* w, float* wgap, float* werr, float* work, blasint* iwork, float* pivmin, float* spdiam, blasint* twist, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slarrb,SLARRB)))));
#else
void slarrb(blasint* n, float* d, float* lld, blasint* ifirst, blasint* ilast, float* rtol1, float* rtol2, blasint* offset, float* w, float* wgap, float* werr, float* work, blasint* iwork, float* pivmin, float* spdiam, blasint* twist, blasint* info){ FC_GLOBAL(slarrb,SLARRB)((void*) n, (void*) d, (void*) lld, (void*) ifirst, (void*) ilast, (void*) rtol1, (void*) rtol2, (void*) offset, (void*) w, (void*) wgap, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) twist, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slarrb_(void* n, void* d, void* lld, void* ifirst, void* ilast, void* rtol1, void* rtol2, void* offset, void* w, void* wgap, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* twist, void* info)
{
    void (*fn) (void* n, void* d, void* lld, void* ifirst, void* ilast, void* rtol1, void* rtol2, void* offset, void* w, void* wgap, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* twist, void* info);

    *(void **) & fn = current_backend->lapack.slarrb.f77_blas_function;

    fn((void*) n, (void*) d, (void*) lld, (void*) ifirst, (void*) ilast, (void*) rtol1, (void*) rtol2, (void*) offset, (void*) w, (void*) wgap, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) twist, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slarrb(void* n, void* d, void* lld, void* ifirst, void* ilast, void* rtol1, void* rtol2, void* offset, void* w, void* wgap, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* twist, void* info) __attribute__((alias("flexiblas_real_slarrb_")));
#else
void flexiblas_real_slarrb(void* n, void* d, void* lld, void* ifirst, void* ilast, void* rtol1, void* rtol2, void* offset, void* w, void* wgap, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* twist, void* info){flexiblas_real_slarrb_((void*) n, (void*) d, (void*) lld, (void*) ifirst, (void*) ilast, (void*) rtol1, (void*) rtol2, (void*) offset, (void*) w, (void*) wgap, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) twist, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slarrb_(void* n, void* d, void* lld, void* ifirst, void* ilast, void* rtol1, void* rtol2, void* offset, void* w, void* wgap, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* twist, void* info)
{
    void (*fn) (void* n, void* d, void* lld, void* ifirst, void* ilast, void* rtol1, void* rtol2, void* offset, void* w, void* wgap, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* twist, void* info);
    void (*fn_hook) (void* n, void* d, void* lld, void* ifirst, void* ilast, void* rtol1, void* rtol2, void* offset, void* w, void* wgap, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* twist, void* info);

    *(void **) &fn      = current_backend->lapack.slarrb.f77_blas_function;

    hook_pos_slarrb ++;
    if( hook_pos_slarrb < __flexiblas_hooks->slarrb.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slarrb.f77_hook_function[hook_pos_slarrb];
        fn_hook((void*) n, (void*) d, (void*) lld, (void*) ifirst, (void*) ilast, (void*) rtol1, (void*) rtol2, (void*) offset, (void*) w, (void*) wgap, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) twist, (void*) info);
    } else {
        hook_pos_slarrb = 0;
        fn((void*) n, (void*) d, (void*) lld, (void*) ifirst, (void*) ilast, (void*) rtol1, (void*) rtol2, (void*) offset, (void*) w, (void*) wgap, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) twist, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slarrb(void* n, void* d, void* lld, void* ifirst, void* ilast, void* rtol1, void* rtol2, void* offset, void* w, void* wgap, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* twist, void* info) __attribute__((alias("flexiblas_chain_slarrb_")));
#else
void flexiblas_chain_slarrb(void* n, void* d, void* lld, void* ifirst, void* ilast, void* rtol1, void* rtol2, void* offset, void* w, void* wgap, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* twist, void* info){flexiblas_chain_slarrb_((void*) n, (void*) d, (void*) lld, (void*) ifirst, (void*) ilast, (void*) rtol1, (void*) rtol2, (void*) offset, (void*) w, (void*) wgap, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) twist, (void*) info);}
#endif



