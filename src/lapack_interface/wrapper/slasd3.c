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


static TLS_STORE uint8_t hook_pos_slasd3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasd3,SLASD3)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* q, blasint* ldq, float* dsigma, float* u, blasint* ldu, float* u2, blasint* ldu2, float* vt, blasint* ldvt, float* vt2, blasint* ldvt2, blasint* idxc, blasint* ctot, float* z, blasint* info)
#else
void FC_GLOBAL(slasd3,SLASD3)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* q, blasint* ldq, float* dsigma, float* u, blasint* ldu, float* u2, blasint* ldu2, float* vt, blasint* ldvt, float* vt2, blasint* ldvt2, blasint* idxc, blasint* ctot, float* z, blasint* info)
#endif
{
    void (*fn) (void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info);
    void (*fn_hook) (void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slasd3.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slasd3.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info);
        return;
    } else {
        hook_pos_slasd3 = 0;
        fn_hook((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slasd3,SLASD3)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* q, blasint* ldq, float* dsigma, float* u, blasint* ldu, float* u2, blasint* ldu2, float* vt, blasint* ldvt, float* vt2, blasint* ldvt2, blasint* idxc, blasint* ctot, float* z, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slasd3,SLASD3)))));
void FC_GLOBAL3(slasd3,SLASD3)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* q, blasint* ldq, float* dsigma, float* u, blasint* ldu, float* u2, blasint* ldu2, float* vt, blasint* ldvt, float* vt2, blasint* ldvt2, blasint* idxc, blasint* ctot, float* z, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slasd3,SLASD3)))));
#else
void FC_GLOBAL2(slasd3,SLASD3)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* q, blasint* ldq, float* dsigma, float* u, blasint* ldu, float* u2, blasint* ldu2, float* vt, blasint* ldvt, float* vt2, blasint* ldvt2, blasint* idxc, blasint* ctot, float* z, blasint* info){ FC_GLOBAL(slasd3,SLASD3)((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info); }
void FC_GLOBAL3(slasd3,SLASD3)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* q, blasint* ldq, float* dsigma, float* u, blasint* ldu, float* u2, blasint* ldu2, float* vt, blasint* ldvt, float* vt2, blasint* ldvt2, blasint* idxc, blasint* ctot, float* z, blasint* info){ FC_GLOBAL(slasd3,SLASD3)((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasd3_(void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info)
{
    void (*fn) (void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info);

    *(void **) & fn = current_backend->lapack.slasd3.f77_blas_function;

    fn((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slasd3(void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info) __attribute__((alias("flexiblas_real_slasd3_")));
#else
void flexiblas_real_slasd3(void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info){flexiblas_real_slasd3_((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slasd3_(void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info)
{
    void (*fn) (void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info);
    void (*fn_hook) (void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info);

    *(void **) &fn      = current_backend->lapack.slasd3.f77_blas_function;

    hook_pos_slasd3 ++;
    if( hook_pos_slasd3 < __flexiblas_hooks->slasd3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slasd3.f77_hook_function[hook_pos_slasd3];
        fn_hook((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info);
    } else {
        hook_pos_slasd3 = 0;
        fn((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slasd3(void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info) __attribute__((alias("flexiblas_chain_slasd3_")));
#else
void flexiblas_chain_slasd3(void* nl, void* nr, void* sqre, void* k, void* d, void* q, void* ldq, void* dsigma, void* u, void* ldu, void* u2, void* ldu2, void* vt, void* ldvt, void* vt2, void* ldvt2, void* idxc, void* ctot, void* z, void* info){flexiblas_chain_slasd3_((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) q, (void*) ldq, (void*) dsigma, (void*) u, (void*) ldu, (void*) u2, (void*) ldu2, (void*) vt, (void*) ldvt, (void*) vt2, (void*) ldvt2, (void*) idxc, (void*) ctot, (void*) z, (void*) info);}
#endif



