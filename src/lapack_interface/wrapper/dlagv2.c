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


static TLS_STORE uint8_t hook_pos_dlagv2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlagv2,DLAGV2)(double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* csl, double* snl, double* csr, double* snr)
#else
void FC_GLOBAL(dlagv2,DLAGV2)(double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* csl, double* snl, double* csr, double* snr)
#endif
{
    void (*fn) (void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr);
    void (*fn_hook) (void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlagv2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlagv2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr);
        return;
    } else {
        hook_pos_dlagv2 = 0;
        fn_hook((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlagv2,DLAGV2)(double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* csl, double* snl, double* csr, double* snr) __attribute__((alias(MTS(FC_GLOBAL(dlagv2,DLAGV2)))));
void FC_GLOBAL3(dlagv2,DLAGV2)(double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* csl, double* snl, double* csr, double* snr) __attribute__((alias(MTS(FC_GLOBAL(dlagv2,DLAGV2)))));
#else
void FC_GLOBAL2(dlagv2,DLAGV2)(double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* csl, double* snl, double* csr, double* snr){ FC_GLOBAL(dlagv2,DLAGV2)((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr); }
void FC_GLOBAL3(dlagv2,DLAGV2)(double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* csl, double* snl, double* csr, double* snr){ FC_GLOBAL(dlagv2,DLAGV2)((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlagv2_(void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr)
{
    void (*fn) (void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr);

    *(void **) & fn = current_backend->lapack.dlagv2.f77_blas_function;

    fn((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlagv2(void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr) __attribute__((alias("flexiblas_real_dlagv2_")));
#else
void flexiblas_real_dlagv2(void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr){flexiblas_real_dlagv2_((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlagv2_(void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr)
{
    void (*fn) (void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr);
    void (*fn_hook) (void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr);

    *(void **) &fn      = current_backend->lapack.dlagv2.f77_blas_function;

    hook_pos_dlagv2 ++;
    if( hook_pos_dlagv2 < __flexiblas_hooks->dlagv2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlagv2.f77_hook_function[hook_pos_dlagv2];
        fn_hook((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr);
    } else {
        hook_pos_dlagv2 = 0;
        fn((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlagv2(void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr) __attribute__((alias("flexiblas_chain_dlagv2_")));
#else
void flexiblas_chain_dlagv2(void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr){flexiblas_chain_dlagv2_((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr);}
#endif



