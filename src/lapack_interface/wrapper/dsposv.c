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


static TLS_STORE uint8_t hook_pos_dsposv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dsposv,DSPOSV)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(dsposv,DSPOSV)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    void (*fn) (void* uplo, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dsposv.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dsposv.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    } else {
        hook_pos_dsposv = 0;
        fn_hook((void*) uplo, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dsposv,DSPOSV)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(dsposv,DSPOSV)))));
void FC_GLOBAL3(dsposv,DSPOSV)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(dsposv,DSPOSV)))));
#else
void FC_GLOBAL2(dsposv,DSPOSV)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(dsposv,DSPOSV)((void*) uplo, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
void FC_GLOBAL3(dsposv,DSPOSV)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(dsposv,DSPOSV)((void*) uplo, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dsposv_(void* uplo, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) & fn = current_backend->lapack.dsposv.f77_blas_function;

    fn((void*) uplo, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dsposv(void* uplo, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_dsposv_")));
#else
void flexiblas_real_dsposv(void* uplo, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_dsposv_((void*) uplo, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dsposv_(void* uplo, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) &fn      = current_backend->lapack.dsposv.f77_blas_function;

    hook_pos_dsposv ++;
    if( hook_pos_dsposv < __flexiblas_hooks->dsposv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dsposv.f77_hook_function[hook_pos_dsposv];
        fn_hook((void*) uplo, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_dsposv = 0;
        fn((void*) uplo, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dsposv(void* uplo, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_dsposv_")));
#else
void flexiblas_chain_dsposv(void* uplo, void* n, void* nrhs, void* a, void* lda, void* b, void* ldb, void* x, void* ldx, void* work, void* swork, void* iter, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_dsposv_((void*) uplo, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) work, (void*) swork, (void*) iter, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



