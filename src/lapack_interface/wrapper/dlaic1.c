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


static TLS_STORE uint8_t hook_pos_dlaic1 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlaic1,DLAIC1)(blasint* job, blasint* j, double* x, double* sest, double* w, double* gamma, double* sestpr, double* s, double* c)
#else
void FC_GLOBAL(dlaic1,DLAIC1)(blasint* job, blasint* j, double* x, double* sest, double* w, double* gamma, double* sestpr, double* s, double* c)
#endif
{
    void (*fn) (void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c);
    void (*fn_hook) (void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlaic1.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlaic1.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);
        return;
    } else {
        hook_pos_dlaic1 = 0;
        fn_hook((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlaic1,DLAIC1)(blasint* job, blasint* j, double* x, double* sest, double* w, double* gamma, double* sestpr, double* s, double* c) __attribute__((alias(MTS(FC_GLOBAL(dlaic1,DLAIC1)))));
void FC_GLOBAL3(dlaic1,DLAIC1)(blasint* job, blasint* j, double* x, double* sest, double* w, double* gamma, double* sestpr, double* s, double* c) __attribute__((alias(MTS(FC_GLOBAL(dlaic1,DLAIC1)))));
#else
void FC_GLOBAL2(dlaic1,DLAIC1)(blasint* job, blasint* j, double* x, double* sest, double* w, double* gamma, double* sestpr, double* s, double* c){ FC_GLOBAL(dlaic1,DLAIC1)((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c); }
void FC_GLOBAL3(dlaic1,DLAIC1)(blasint* job, blasint* j, double* x, double* sest, double* w, double* gamma, double* sestpr, double* s, double* c){ FC_GLOBAL(dlaic1,DLAIC1)((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlaic1_(void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c)
{
    void (*fn) (void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c);

    *(void **) & fn = current_backend->lapack.dlaic1.f77_blas_function;

    fn((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlaic1(void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c) __attribute__((alias("flexiblas_real_dlaic1_")));
#else
void flexiblas_real_dlaic1(void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c){flexiblas_real_dlaic1_((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlaic1_(void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c)
{
    void (*fn) (void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c);
    void (*fn_hook) (void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c);

    *(void **) &fn      = current_backend->lapack.dlaic1.f77_blas_function;

    hook_pos_dlaic1 ++;
    if( hook_pos_dlaic1 < __flexiblas_hooks->dlaic1.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlaic1.f77_hook_function[hook_pos_dlaic1];
        fn_hook((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);
    } else {
        hook_pos_dlaic1 = 0;
        fn((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlaic1(void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c) __attribute__((alias("flexiblas_chain_dlaic1_")));
#else
void flexiblas_chain_dlaic1(void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c){flexiblas_chain_dlaic1_((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);}
#endif



