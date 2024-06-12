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


static TLS_STORE uint8_t hook_pos_dlasy2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlasy2,DLASY2)(blasint* ltranl, blasint* ltranr, blasint* isgn, blasint* n1, blasint* n2, double* tl, blasint* ldtl, double* tr, blasint* ldtr, double* b, blasint* ldb, double* scale, double* x, blasint* ldx, double* xnorm, blasint* info)
#else
void FC_GLOBAL(dlasy2,DLASY2)(blasint* ltranl, blasint* ltranr, blasint* isgn, blasint* n1, blasint* n2, double* tl, blasint* ldtl, double* tr, blasint* ldtr, double* b, blasint* ldb, double* scale, double* x, blasint* ldx, double* xnorm, blasint* info)
#endif
{
    void (*fn) (void* ltranl, void* ltranr, void* isgn, void* n1, void* n2, void* tl, void* ldtl, void* tr, void* ldtr, void* b, void* ldb, void* scale, void* x, void* ldx, void* xnorm, void* info);
    void (*fn_hook) (void* ltranl, void* ltranr, void* isgn, void* n1, void* n2, void* tl, void* ldtl, void* tr, void* ldtr, void* b, void* ldb, void* scale, void* x, void* ldx, void* xnorm, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlasy2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlasy2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) ltranl, (void*) ltranr, (void*) isgn, (void*) n1, (void*) n2, (void*) tl, (void*) ldtl, (void*) tr, (void*) ldtr, (void*) b, (void*) ldb, (void*) scale, (void*) x, (void*) ldx, (void*) xnorm, (void*) info);
        return;
    } else {
        hook_pos_dlasy2 = 0;
        fn_hook((void*) ltranl, (void*) ltranr, (void*) isgn, (void*) n1, (void*) n2, (void*) tl, (void*) ldtl, (void*) tr, (void*) ldtr, (void*) b, (void*) ldb, (void*) scale, (void*) x, (void*) ldx, (void*) xnorm, (void*) info);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dlasy2_(blasint* ltranl, blasint* ltranr, blasint* isgn, blasint* n1, blasint* n2, double* tl, blasint* ldtl, double* tr, blasint* ldtr, double* b, blasint* ldb, double* scale, double* x, blasint* ldx, double* xnorm, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasy2,DLASY2)))));
#else
#ifndef __APPLE__
void dlasy2(blasint* ltranl, blasint* ltranr, blasint* isgn, blasint* n1, blasint* n2, double* tl, blasint* ldtl, double* tr, blasint* ldtr, double* b, blasint* ldb, double* scale, double* x, blasint* ldx, double* xnorm, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasy2,DLASY2)))));
#else
void dlasy2(blasint* ltranl, blasint* ltranr, blasint* isgn, blasint* n1, blasint* n2, double* tl, blasint* ldtl, double* tr, blasint* ldtr, double* b, blasint* ldb, double* scale, double* x, blasint* ldx, double* xnorm, blasint* info){ FC_GLOBAL(dlasy2,DLASY2)((void*) ltranl, (void*) ltranr, (void*) isgn, (void*) n1, (void*) n2, (void*) tl, (void*) ldtl, (void*) tr, (void*) ldtr, (void*) b, (void*) ldb, (void*) scale, (void*) x, (void*) ldx, (void*) xnorm, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlasy2_(void* ltranl, void* ltranr, void* isgn, void* n1, void* n2, void* tl, void* ldtl, void* tr, void* ldtr, void* b, void* ldb, void* scale, void* x, void* ldx, void* xnorm, void* info)
{
    void (*fn) (void* ltranl, void* ltranr, void* isgn, void* n1, void* n2, void* tl, void* ldtl, void* tr, void* ldtr, void* b, void* ldb, void* scale, void* x, void* ldx, void* xnorm, void* info);

    *(void **) & fn = current_backend->lapack.dlasy2.f77_blas_function;

    fn((void*) ltranl, (void*) ltranr, (void*) isgn, (void*) n1, (void*) n2, (void*) tl, (void*) ldtl, (void*) tr, (void*) ldtr, (void*) b, (void*) ldb, (void*) scale, (void*) x, (void*) ldx, (void*) xnorm, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlasy2(void* ltranl, void* ltranr, void* isgn, void* n1, void* n2, void* tl, void* ldtl, void* tr, void* ldtr, void* b, void* ldb, void* scale, void* x, void* ldx, void* xnorm, void* info) __attribute__((alias("flexiblas_real_dlasy2_")));
#else
void flexiblas_real_dlasy2(void* ltranl, void* ltranr, void* isgn, void* n1, void* n2, void* tl, void* ldtl, void* tr, void* ldtr, void* b, void* ldb, void* scale, void* x, void* ldx, void* xnorm, void* info){flexiblas_real_dlasy2_((void*) ltranl, (void*) ltranr, (void*) isgn, (void*) n1, (void*) n2, (void*) tl, (void*) ldtl, (void*) tr, (void*) ldtr, (void*) b, (void*) ldb, (void*) scale, (void*) x, (void*) ldx, (void*) xnorm, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlasy2_(void* ltranl, void* ltranr, void* isgn, void* n1, void* n2, void* tl, void* ldtl, void* tr, void* ldtr, void* b, void* ldb, void* scale, void* x, void* ldx, void* xnorm, void* info)
{
    void (*fn) (void* ltranl, void* ltranr, void* isgn, void* n1, void* n2, void* tl, void* ldtl, void* tr, void* ldtr, void* b, void* ldb, void* scale, void* x, void* ldx, void* xnorm, void* info);
    void (*fn_hook) (void* ltranl, void* ltranr, void* isgn, void* n1, void* n2, void* tl, void* ldtl, void* tr, void* ldtr, void* b, void* ldb, void* scale, void* x, void* ldx, void* xnorm, void* info);

    *(void **) &fn      = current_backend->lapack.dlasy2.f77_blas_function;

    hook_pos_dlasy2 ++;
    if( hook_pos_dlasy2 < __flexiblas_hooks->dlasy2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlasy2.f77_hook_function[hook_pos_dlasy2];
        fn_hook((void*) ltranl, (void*) ltranr, (void*) isgn, (void*) n1, (void*) n2, (void*) tl, (void*) ldtl, (void*) tr, (void*) ldtr, (void*) b, (void*) ldb, (void*) scale, (void*) x, (void*) ldx, (void*) xnorm, (void*) info);
    } else {
        hook_pos_dlasy2 = 0;
        fn((void*) ltranl, (void*) ltranr, (void*) isgn, (void*) n1, (void*) n2, (void*) tl, (void*) ldtl, (void*) tr, (void*) ldtr, (void*) b, (void*) ldb, (void*) scale, (void*) x, (void*) ldx, (void*) xnorm, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlasy2(void* ltranl, void* ltranr, void* isgn, void* n1, void* n2, void* tl, void* ldtl, void* tr, void* ldtr, void* b, void* ldb, void* scale, void* x, void* ldx, void* xnorm, void* info) __attribute__((alias("flexiblas_chain_dlasy2_")));
#else
void flexiblas_chain_dlasy2(void* ltranl, void* ltranr, void* isgn, void* n1, void* n2, void* tl, void* ldtl, void* tr, void* ldtr, void* b, void* ldb, void* scale, void* x, void* ldx, void* xnorm, void* info){flexiblas_chain_dlasy2_((void*) ltranl, (void*) ltranr, (void*) isgn, (void*) n1, (void*) n2, (void*) tl, (void*) ldtl, (void*) tr, (void*) ldtr, (void*) b, (void*) ldb, (void*) scale, (void*) x, (void*) ldx, (void*) xnorm, (void*) info);}
#endif



