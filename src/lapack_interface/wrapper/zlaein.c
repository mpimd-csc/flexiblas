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


static TLS_STORE uint8_t hook_pos_zlaein = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlaein,ZLAEIN)(blasint* rightv, blasint* noinit, blasint* n, double complex* h, blasint* ldh, double complex* w, double complex* v, double complex* b, blasint* ldb, double* rwork, double* eps3, double* smlnum, blasint* info)
#else
void FC_GLOBAL(zlaein,ZLAEIN)(blasint* rightv, blasint* noinit, blasint* n, double complex* h, blasint* ldh, double complex* w, double complex* v, double complex* b, blasint* ldb, double* rwork, double* eps3, double* smlnum, blasint* info)
#endif
{
    void (*fn) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info);
    void (*fn_hook) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlaein.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlaein.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info);
        return;
    } else {
        hook_pos_zlaein = 0;
        fn_hook((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void zlaein_(blasint* rightv, blasint* noinit, blasint* n, double complex* h, blasint* ldh, double complex* w, double complex* v, double complex* b, blasint* ldb, double* rwork, double* eps3, double* smlnum, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zlaein,ZLAEIN)))));
#else
#ifndef __APPLE__
void zlaein(blasint* rightv, blasint* noinit, blasint* n, double complex* h, blasint* ldh, double complex* w, double complex* v, double complex* b, blasint* ldb, double* rwork, double* eps3, double* smlnum, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zlaein,ZLAEIN)))));
#else
void zlaein(blasint* rightv, blasint* noinit, blasint* n, double complex* h, blasint* ldh, double complex* w, double complex* v, double complex* b, blasint* ldb, double* rwork, double* eps3, double* smlnum, blasint* info){ FC_GLOBAL(zlaein,ZLAEIN)((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaein_(void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info)
{
    void (*fn) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info);

    *(void **) & fn = current_backend->lapack.zlaein.f77_blas_function;

    fn((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlaein(void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info) __attribute__((alias("flexiblas_real_zlaein_")));
#else
void flexiblas_real_zlaein(void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info){flexiblas_real_zlaein_((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlaein_(void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info)
{
    void (*fn) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info);
    void (*fn_hook) (void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info);

    *(void **) &fn      = current_backend->lapack.zlaein.f77_blas_function;

    hook_pos_zlaein ++;
    if( hook_pos_zlaein < __flexiblas_hooks->zlaein.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlaein.f77_hook_function[hook_pos_zlaein];
        fn_hook((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info);
    } else {
        hook_pos_zlaein = 0;
        fn((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlaein(void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info) __attribute__((alias("flexiblas_chain_zlaein_")));
#else
void flexiblas_chain_zlaein(void* rightv, void* noinit, void* n, void* h, void* ldh, void* w, void* v, void* b, void* ldb, void* rwork, void* eps3, void* smlnum, void* info){flexiblas_chain_zlaein_((void*) rightv, (void*) noinit, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) v, (void*) b, (void*) ldb, (void*) rwork, (void*) eps3, (void*) smlnum, (void*) info);}
#endif



