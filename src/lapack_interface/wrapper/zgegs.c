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


static TLS_STORE uint8_t hook_pos_zgegs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgegs,ZGEGS)(char* jobvsl, char* jobvsr, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* alpha, double complex* beta, double complex* vsl, blasint* ldvsl, double complex* vsr, blasint* ldvsr, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr)
#else
void FC_GLOBAL(zgegs,ZGEGS)(char* jobvsl, char* jobvsr, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* alpha, double complex* beta, double complex* vsl, blasint* ldvsl, double complex* vsr, blasint* ldvsr, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr)
#endif
{
    void (*fn) (void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);
    void (*fn_hook) (void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zgegs.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zgegs.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr);
        return;
    } else {
        hook_pos_zgegs = 0;
        fn_hook((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zgegs,ZGEGS)(char* jobvsl, char* jobvsr, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* alpha, double complex* beta, double complex* vsl, blasint* ldvsl, double complex* vsr, blasint* ldvsr, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr) __attribute__((alias(MTS(FC_GLOBAL(zgegs,ZGEGS)))));
void FC_GLOBAL3(zgegs,ZGEGS)(char* jobvsl, char* jobvsr, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* alpha, double complex* beta, double complex* vsl, blasint* ldvsl, double complex* vsr, blasint* ldvsr, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr) __attribute__((alias(MTS(FC_GLOBAL(zgegs,ZGEGS)))));
#else
void FC_GLOBAL2(zgegs,ZGEGS)(char* jobvsl, char* jobvsr, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* alpha, double complex* beta, double complex* vsl, blasint* ldvsl, double complex* vsr, blasint* ldvsr, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr){ FC_GLOBAL(zgegs,ZGEGS)((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr); }
void FC_GLOBAL3(zgegs,ZGEGS)(char* jobvsl, char* jobvsr, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* alpha, double complex* beta, double complex* vsl, blasint* ldvsl, double complex* vsr, blasint* ldvsr, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr){ FC_GLOBAL(zgegs,ZGEGS)((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgegs_(void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr)
{
    void (*fn) (void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);

    *(void **) & fn = current_backend->lapack.zgegs.f77_blas_function;

    fn((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zgegs(void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr) __attribute__((alias("flexiblas_real_zgegs_")));
#else
void flexiblas_real_zgegs(void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr){flexiblas_real_zgegs_((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgegs_(void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr)
{
    void (*fn) (void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);
    void (*fn_hook) (void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);

    *(void **) &fn      = current_backend->lapack.zgegs.f77_blas_function;

    hook_pos_zgegs ++;
    if( hook_pos_zgegs < __flexiblas_hooks->zgegs.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgegs.f77_hook_function[hook_pos_zgegs];
        fn_hook((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr);
    } else {
        hook_pos_zgegs = 0;
        fn((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zgegs(void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr) __attribute__((alias("flexiblas_chain_zgegs_")));
#else
void flexiblas_chain_zgegs(void* jobvsl, void* jobvsr, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr){flexiblas_chain_zgegs_((void*) jobvsl, (void*) jobvsr, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr);}
#endif



