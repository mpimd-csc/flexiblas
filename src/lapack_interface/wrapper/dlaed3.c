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


static TLS_STORE uint8_t hook_pos_dlaed3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlaed3,DLAED3)(blasint* k, blasint* n, blasint* n1, double* d, double* q, blasint* ldq, double* rho, double* dlambda, double* q2, blasint* indx, blasint* ctot, double* w, double* s, blasint* info)
#else
void FC_GLOBAL(dlaed3,DLAED3)(blasint* k, blasint* n, blasint* n1, double* d, double* q, blasint* ldq, double* rho, double* dlambda, double* q2, blasint* indx, blasint* ctot, double* w, double* s, blasint* info)
#endif
{
    void (*fn) (void* k, void* n, void* n1, void* d, void* q, void* ldq, void* rho, void* dlambda, void* q2, void* indx, void* ctot, void* w, void* s, void* info);
    void (*fn_hook) (void* k, void* n, void* n1, void* d, void* q, void* ldq, void* rho, void* dlambda, void* q2, void* indx, void* ctot, void* w, void* s, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlaed3.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlaed3.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) k, (void*) n, (void*) n1, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) q2, (void*) indx, (void*) ctot, (void*) w, (void*) s, (void*) info);
        return;
    } else {
        hook_pos_dlaed3 = 0;
        fn_hook((void*) k, (void*) n, (void*) n1, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) q2, (void*) indx, (void*) ctot, (void*) w, (void*) s, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlaed3,DLAED3)(blasint* k, blasint* n, blasint* n1, double* d, double* q, blasint* ldq, double* rho, double* dlambda, double* q2, blasint* indx, blasint* ctot, double* w, double* s, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlaed3,DLAED3)))));
void FC_GLOBAL3(dlaed3,DLAED3)(blasint* k, blasint* n, blasint* n1, double* d, double* q, blasint* ldq, double* rho, double* dlambda, double* q2, blasint* indx, blasint* ctot, double* w, double* s, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlaed3,DLAED3)))));
#else
void FC_GLOBAL2(dlaed3,DLAED3)(blasint* k, blasint* n, blasint* n1, double* d, double* q, blasint* ldq, double* rho, double* dlambda, double* q2, blasint* indx, blasint* ctot, double* w, double* s, blasint* info){ FC_GLOBAL(dlaed3,DLAED3)((void*) k, (void*) n, (void*) n1, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) q2, (void*) indx, (void*) ctot, (void*) w, (void*) s, (void*) info); }
void FC_GLOBAL3(dlaed3,DLAED3)(blasint* k, blasint* n, blasint* n1, double* d, double* q, blasint* ldq, double* rho, double* dlambda, double* q2, blasint* indx, blasint* ctot, double* w, double* s, blasint* info){ FC_GLOBAL(dlaed3,DLAED3)((void*) k, (void*) n, (void*) n1, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) q2, (void*) indx, (void*) ctot, (void*) w, (void*) s, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlaed3_(void* k, void* n, void* n1, void* d, void* q, void* ldq, void* rho, void* dlambda, void* q2, void* indx, void* ctot, void* w, void* s, void* info)
{
    void (*fn) (void* k, void* n, void* n1, void* d, void* q, void* ldq, void* rho, void* dlambda, void* q2, void* indx, void* ctot, void* w, void* s, void* info);

    *(void **) & fn = current_backend->lapack.dlaed3.f77_blas_function;

    fn((void*) k, (void*) n, (void*) n1, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) q2, (void*) indx, (void*) ctot, (void*) w, (void*) s, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlaed3(void* k, void* n, void* n1, void* d, void* q, void* ldq, void* rho, void* dlambda, void* q2, void* indx, void* ctot, void* w, void* s, void* info) __attribute__((alias("flexiblas_real_dlaed3_")));
#else
void flexiblas_real_dlaed3(void* k, void* n, void* n1, void* d, void* q, void* ldq, void* rho, void* dlambda, void* q2, void* indx, void* ctot, void* w, void* s, void* info){flexiblas_real_dlaed3_((void*) k, (void*) n, (void*) n1, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) q2, (void*) indx, (void*) ctot, (void*) w, (void*) s, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlaed3_(void* k, void* n, void* n1, void* d, void* q, void* ldq, void* rho, void* dlambda, void* q2, void* indx, void* ctot, void* w, void* s, void* info)
{
    void (*fn) (void* k, void* n, void* n1, void* d, void* q, void* ldq, void* rho, void* dlambda, void* q2, void* indx, void* ctot, void* w, void* s, void* info);
    void (*fn_hook) (void* k, void* n, void* n1, void* d, void* q, void* ldq, void* rho, void* dlambda, void* q2, void* indx, void* ctot, void* w, void* s, void* info);

    *(void **) &fn      = current_backend->lapack.dlaed3.f77_blas_function;

    hook_pos_dlaed3 ++;
    if( hook_pos_dlaed3 < __flexiblas_hooks->dlaed3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlaed3.f77_hook_function[hook_pos_dlaed3];
        fn_hook((void*) k, (void*) n, (void*) n1, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) q2, (void*) indx, (void*) ctot, (void*) w, (void*) s, (void*) info);
    } else {
        hook_pos_dlaed3 = 0;
        fn((void*) k, (void*) n, (void*) n1, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) q2, (void*) indx, (void*) ctot, (void*) w, (void*) s, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlaed3(void* k, void* n, void* n1, void* d, void* q, void* ldq, void* rho, void* dlambda, void* q2, void* indx, void* ctot, void* w, void* s, void* info) __attribute__((alias("flexiblas_chain_dlaed3_")));
#else
void flexiblas_chain_dlaed3(void* k, void* n, void* n1, void* d, void* q, void* ldq, void* rho, void* dlambda, void* q2, void* indx, void* ctot, void* w, void* s, void* info){flexiblas_chain_dlaed3_((void*) k, (void*) n, (void*) n1, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) q2, (void*) indx, (void*) ctot, (void*) w, (void*) s, (void*) info);}
#endif



