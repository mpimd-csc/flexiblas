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


static TLS_STORE uint8_t hook_pos_dlarrk = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlarrk,DLARRK)(blasint* n, blasint* iw, double* gl, double* gu, double* d, double* e2, double* pivmin, double* reltol, double* w, double* werr, blasint* info)
#else
void FC_GLOBAL(dlarrk,DLARRK)(blasint* n, blasint* iw, double* gl, double* gu, double* d, double* e2, double* pivmin, double* reltol, double* w, double* werr, blasint* info)
#endif
{
    void (*fn) (void* n, void* iw, void* gl, void* gu, void* d, void* e2, void* pivmin, void* reltol, void* w, void* werr, void* info);
    void (*fn_hook) (void* n, void* iw, void* gl, void* gu, void* d, void* e2, void* pivmin, void* reltol, void* w, void* werr, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlarrk.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlarrk.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) iw, (void*) gl, (void*) gu, (void*) d, (void*) e2, (void*) pivmin, (void*) reltol, (void*) w, (void*) werr, (void*) info);
        return;
    } else {
        hook_pos_dlarrk = 0;
        fn_hook((void*) n, (void*) iw, (void*) gl, (void*) gu, (void*) d, (void*) e2, (void*) pivmin, (void*) reltol, (void*) w, (void*) werr, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlarrk,DLARRK)(blasint* n, blasint* iw, double* gl, double* gu, double* d, double* e2, double* pivmin, double* reltol, double* w, double* werr, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlarrk,DLARRK)))));
void FC_GLOBAL3(dlarrk,DLARRK)(blasint* n, blasint* iw, double* gl, double* gu, double* d, double* e2, double* pivmin, double* reltol, double* w, double* werr, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlarrk,DLARRK)))));
#else
void FC_GLOBAL2(dlarrk,DLARRK)(blasint* n, blasint* iw, double* gl, double* gu, double* d, double* e2, double* pivmin, double* reltol, double* w, double* werr, blasint* info){ FC_GLOBAL(dlarrk,DLARRK)((void*) n, (void*) iw, (void*) gl, (void*) gu, (void*) d, (void*) e2, (void*) pivmin, (void*) reltol, (void*) w, (void*) werr, (void*) info); }
void FC_GLOBAL3(dlarrk,DLARRK)(blasint* n, blasint* iw, double* gl, double* gu, double* d, double* e2, double* pivmin, double* reltol, double* w, double* werr, blasint* info){ FC_GLOBAL(dlarrk,DLARRK)((void*) n, (void*) iw, (void*) gl, (void*) gu, (void*) d, (void*) e2, (void*) pivmin, (void*) reltol, (void*) w, (void*) werr, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlarrk_(void* n, void* iw, void* gl, void* gu, void* d, void* e2, void* pivmin, void* reltol, void* w, void* werr, void* info)
{
    void (*fn) (void* n, void* iw, void* gl, void* gu, void* d, void* e2, void* pivmin, void* reltol, void* w, void* werr, void* info);

    *(void **) & fn = current_backend->lapack.dlarrk.f77_blas_function;

    fn((void*) n, (void*) iw, (void*) gl, (void*) gu, (void*) d, (void*) e2, (void*) pivmin, (void*) reltol, (void*) w, (void*) werr, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlarrk(void* n, void* iw, void* gl, void* gu, void* d, void* e2, void* pivmin, void* reltol, void* w, void* werr, void* info) __attribute__((alias("flexiblas_real_dlarrk_")));
#else
void flexiblas_real_dlarrk(void* n, void* iw, void* gl, void* gu, void* d, void* e2, void* pivmin, void* reltol, void* w, void* werr, void* info){flexiblas_real_dlarrk_((void*) n, (void*) iw, (void*) gl, (void*) gu, (void*) d, (void*) e2, (void*) pivmin, (void*) reltol, (void*) w, (void*) werr, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlarrk_(void* n, void* iw, void* gl, void* gu, void* d, void* e2, void* pivmin, void* reltol, void* w, void* werr, void* info)
{
    void (*fn) (void* n, void* iw, void* gl, void* gu, void* d, void* e2, void* pivmin, void* reltol, void* w, void* werr, void* info);
    void (*fn_hook) (void* n, void* iw, void* gl, void* gu, void* d, void* e2, void* pivmin, void* reltol, void* w, void* werr, void* info);

    *(void **) &fn      = current_backend->lapack.dlarrk.f77_blas_function;

    hook_pos_dlarrk ++;
    if( hook_pos_dlarrk < __flexiblas_hooks->dlarrk.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlarrk.f77_hook_function[hook_pos_dlarrk];
        fn_hook((void*) n, (void*) iw, (void*) gl, (void*) gu, (void*) d, (void*) e2, (void*) pivmin, (void*) reltol, (void*) w, (void*) werr, (void*) info);
    } else {
        hook_pos_dlarrk = 0;
        fn((void*) n, (void*) iw, (void*) gl, (void*) gu, (void*) d, (void*) e2, (void*) pivmin, (void*) reltol, (void*) w, (void*) werr, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlarrk(void* n, void* iw, void* gl, void* gu, void* d, void* e2, void* pivmin, void* reltol, void* w, void* werr, void* info) __attribute__((alias("flexiblas_chain_dlarrk_")));
#else
void flexiblas_chain_dlarrk(void* n, void* iw, void* gl, void* gu, void* d, void* e2, void* pivmin, void* reltol, void* w, void* werr, void* info){flexiblas_chain_dlarrk_((void*) n, (void*) iw, (void*) gl, (void*) gu, (void*) d, (void*) e2, (void*) pivmin, (void*) reltol, (void*) w, (void*) werr, (void*) info);}
#endif



