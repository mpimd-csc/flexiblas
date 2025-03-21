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


static TLS_STORE uint8_t hook_pos_clalsd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clalsd,CLALSD)(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, float complex* b, blasint* ldb, float* rcond, blasint* rank_bn, float complex* work, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(clalsd,CLALSD)(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, float complex* b, blasint* ldb, float* rcond, blasint* rank_bn, float complex* work, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    void (*fn) (void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.clalsd.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->clalsd.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    } else {
        hook_pos_clalsd = 0;
        fn_hook((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(clalsd,CLALSD)(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, float complex* b, blasint* ldb, float* rcond, blasint* rank_bn, float complex* work, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(clalsd,CLALSD)))));
void FC_GLOBAL3(clalsd,CLALSD)(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, float complex* b, blasint* ldb, float* rcond, blasint* rank_bn, float complex* work, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(clalsd,CLALSD)))));
#else
void FC_GLOBAL2(clalsd,CLALSD)(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, float complex* b, blasint* ldb, float* rcond, blasint* rank_bn, float complex* work, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(clalsd,CLALSD)((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) rwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
void FC_GLOBAL3(clalsd,CLALSD)(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, float complex* b, blasint* ldb, float* rcond, blasint* rank_bn, float complex* work, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(clalsd,CLALSD)((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) rwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clalsd_(void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) & fn = current_backend->lapack.clalsd.f77_blas_function;

    fn((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);

    return;
}
#ifndef __APPLE__
void flexiblas_real_clalsd(void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_clalsd_")));
#else
void flexiblas_real_clalsd(void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_clalsd_((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) rwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clalsd_(void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) &fn      = current_backend->lapack.clalsd.f77_blas_function;

    hook_pos_clalsd ++;
    if( hook_pos_clalsd < __flexiblas_hooks->clalsd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clalsd.f77_hook_function[hook_pos_clalsd];
        fn_hook((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_clalsd = 0;
        fn((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_clalsd(void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_clalsd_")));
#else
void flexiblas_chain_clalsd(void* uplo, void* smlsiz, void* n, void* nrhs, void* d, void* e, void* b, void* ldb, void* rcond, void* rank_bn, void* work, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_clalsd_((void*) uplo, (void*) smlsiz, (void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb, (void*) rcond, (void*) rank_bn, (void*) work, (void*) rwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



