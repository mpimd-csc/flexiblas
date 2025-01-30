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


static TLS_STORE uint8_t hook_pos_clags2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clags2,CLAGS2)(blaslogical* upper, float* a1, float complex* a2, float* a3, float* b1, float complex* b2, float* b3, float* csu, float complex* snu, float* csv, float complex* snv, float* csq, float complex* snq)
#else
void FC_GLOBAL(clags2,CLAGS2)(blaslogical* upper, float* a1, float complex* a2, float* a3, float* b1, float complex* b2, float* b3, float* csu, float complex* snu, float* csv, float complex* snv, float* csq, float complex* snq)
#endif
{
    void (*fn) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);
    void (*fn_hook) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.clags2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->clags2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq);
        return;
    } else {
        hook_pos_clags2 = 0;
        fn_hook((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(clags2,CLAGS2)(blaslogical* upper, float* a1, float complex* a2, float* a3, float* b1, float complex* b2, float* b3, float* csu, float complex* snu, float* csv, float complex* snv, float* csq, float complex* snq) __attribute__((alias(MTS(FC_GLOBAL(clags2,CLAGS2)))));
void FC_GLOBAL3(clags2,CLAGS2)(blaslogical* upper, float* a1, float complex* a2, float* a3, float* b1, float complex* b2, float* b3, float* csu, float complex* snu, float* csv, float complex* snv, float* csq, float complex* snq) __attribute__((alias(MTS(FC_GLOBAL(clags2,CLAGS2)))));
#else
void FC_GLOBAL2(clags2,CLAGS2)(blaslogical* upper, float* a1, float complex* a2, float* a3, float* b1, float complex* b2, float* b3, float* csu, float complex* snu, float* csv, float complex* snv, float* csq, float complex* snq){ FC_GLOBAL(clags2,CLAGS2)((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq); }
void FC_GLOBAL3(clags2,CLAGS2)(blaslogical* upper, float* a1, float complex* a2, float* a3, float* b1, float complex* b2, float* b3, float* csu, float complex* snu, float* csv, float complex* snv, float* csq, float complex* snq){ FC_GLOBAL(clags2,CLAGS2)((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clags2_(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)
{
    void (*fn) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);

    *(void **) & fn = current_backend->lapack.clags2.f77_blas_function;

    fn((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq);

    return;
}
#ifndef __APPLE__
void flexiblas_real_clags2(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq) __attribute__((alias("flexiblas_real_clags2_")));
#else
void flexiblas_real_clags2(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq){flexiblas_real_clags2_((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clags2_(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)
{
    void (*fn) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);
    void (*fn_hook) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);

    *(void **) &fn      = current_backend->lapack.clags2.f77_blas_function;

    hook_pos_clags2 ++;
    if( hook_pos_clags2 < __flexiblas_hooks->clags2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clags2.f77_hook_function[hook_pos_clags2];
        fn_hook((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq);
    } else {
        hook_pos_clags2 = 0;
        fn((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_clags2(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq) __attribute__((alias("flexiblas_chain_clags2_")));
#else
void flexiblas_chain_clags2(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq){flexiblas_chain_clags2_((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq);}
#endif



