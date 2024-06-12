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


static TLS_STORE uint8_t hook_pos_zgesdd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgesdd,ZGESDD)(char* jobz, blasint* m, blasint* n, double complex* a, blasint* lda, double* s, double complex* u, blasint* ldu, double complex* vt, blasint* ldvt, double complex* work, blasint* lwork, double* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz)
#else
void FC_GLOBAL(zgesdd,ZGESDD)(char* jobz, blasint* m, blasint* n, double complex* a, blasint* lda, double* s, double complex* u, blasint* ldu, double complex* vt, blasint* ldvt, double complex* work, blasint* lwork, double* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz)
#endif
{
    void (*fn) (void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz);
    void (*fn_hook) (void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zgesdd.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zgesdd.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);
        return;
    } else {
        hook_pos_zgesdd = 0;
        fn_hook((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void zgesdd_(char* jobz, blasint* m, blasint* n, double complex* a, blasint* lda, double* s, double complex* u, blasint* ldu, double complex* vt, blasint* ldvt, double complex* work, blasint* lwork, double* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz) __attribute__((alias(MTS(FC_GLOBAL(zgesdd,ZGESDD)))));
#else
#ifndef __APPLE__
void zgesdd(char* jobz, blasint* m, blasint* n, double complex* a, blasint* lda, double* s, double complex* u, blasint* ldu, double complex* vt, blasint* ldvt, double complex* work, blasint* lwork, double* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz) __attribute__((alias(MTS(FC_GLOBAL(zgesdd,ZGESDD)))));
#else
void zgesdd(char* jobz, blasint* m, blasint* n, double complex* a, blasint* lda, double* s, double complex* u, blasint* ldu, double complex* vt, blasint* ldvt, double complex* work, blasint* lwork, double* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz){ FC_GLOBAL(zgesdd,ZGESDD)((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobz); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgesdd_(void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz)
{
    void (*fn) (void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz);

    *(void **) & fn = current_backend->lapack.zgesdd.f77_blas_function;

    fn((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zgesdd(void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz) __attribute__((alias("flexiblas_real_zgesdd_")));
#else
void flexiblas_real_zgesdd(void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz){flexiblas_real_zgesdd_((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobz);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgesdd_(void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz)
{
    void (*fn) (void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz);
    void (*fn_hook) (void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz);

    *(void **) &fn      = current_backend->lapack.zgesdd.f77_blas_function;

    hook_pos_zgesdd ++;
    if( hook_pos_zgesdd < __flexiblas_hooks->zgesdd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgesdd.f77_hook_function[hook_pos_zgesdd];
        fn_hook((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);
    } else {
        hook_pos_zgesdd = 0;
        fn((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zgesdd(void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz) __attribute__((alias("flexiblas_chain_zgesdd_")));
#else
void flexiblas_chain_zgesdd(void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz){flexiblas_chain_zgesdd_((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobz);}
#endif



