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


static TLS_STORE uint8_t hook_pos_dlartg = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlartg,DLARTG)(float* f, float* g, float* c, float* s, float* r)
#else
void FC_GLOBAL(dlartg,DLARTG)(float* f, float* g, float* c, float* s, float* r)
#endif
{
    void (*fn) (void* f, void* g, void* c, void* s, void* r);
    void (*fn_hook) (void* f, void* g, void* c, void* s, void* r);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlartg.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlartg.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);
        return;
    } else {
        hook_pos_dlartg = 0;
        fn_hook((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlartg,DLARTG)(float* f, float* g, float* c, float* s, float* r) __attribute__((alias(MTS(FC_GLOBAL(dlartg,DLARTG)))));
void FC_GLOBAL3(dlartg,DLARTG)(float* f, float* g, float* c, float* s, float* r) __attribute__((alias(MTS(FC_GLOBAL(dlartg,DLARTG)))));
#else
void FC_GLOBAL2(dlartg,DLARTG)(float* f, float* g, float* c, float* s, float* r){ FC_GLOBAL(dlartg,DLARTG)((void*) f, (void*) g, (void*) c, (void*) s, (void*) r); }
void FC_GLOBAL3(dlartg,DLARTG)(float* f, float* g, float* c, float* s, float* r){ FC_GLOBAL(dlartg,DLARTG)((void*) f, (void*) g, (void*) c, (void*) s, (void*) r); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlartg_(void* f, void* g, void* c, void* s, void* r)
{
    void (*fn) (void* f, void* g, void* c, void* s, void* r);

    *(void **) & fn = current_backend->lapack.dlartg.f77_blas_function;

    fn((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlartg(void* f, void* g, void* c, void* s, void* r) __attribute__((alias("flexiblas_real_dlartg_")));
#else
void flexiblas_real_dlartg(void* f, void* g, void* c, void* s, void* r){flexiblas_real_dlartg_((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlartg_(void* f, void* g, void* c, void* s, void* r)
{
    void (*fn) (void* f, void* g, void* c, void* s, void* r);
    void (*fn_hook) (void* f, void* g, void* c, void* s, void* r);

    *(void **) &fn      = current_backend->lapack.dlartg.f77_blas_function;

    hook_pos_dlartg ++;
    if( hook_pos_dlartg < __flexiblas_hooks->dlartg.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlartg.f77_hook_function[hook_pos_dlartg];
        fn_hook((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);
    } else {
        hook_pos_dlartg = 0;
        fn((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlartg(void* f, void* g, void* c, void* s, void* r) __attribute__((alias("flexiblas_chain_dlartg_")));
#else
void flexiblas_chain_dlartg(void* f, void* g, void* c, void* s, void* r){flexiblas_chain_dlartg_((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);}
#endif



