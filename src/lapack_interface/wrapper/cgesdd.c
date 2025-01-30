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


static TLS_STORE uint8_t hook_pos_cgesdd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgesdd,CGESDD)(char* jobz, blasint* m, blasint* n, float complex* a, blasint* lda, float* s, float complex* u, blasint* ldu, float complex* vt, blasint* ldvt, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz)
#else
void FC_GLOBAL(cgesdd,CGESDD)(char* jobz, blasint* m, blasint* n, float complex* a, blasint* lda, float* s, float complex* u, blasint* ldu, float complex* vt, blasint* ldvt, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz)
#endif
{
    void (*fn) (void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz);
    void (*fn_hook) (void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.cgesdd.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->cgesdd.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);
        return;
    } else {
        hook_pos_cgesdd = 0;
        fn_hook((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(cgesdd,CGESDD)(char* jobz, blasint* m, blasint* n, float complex* a, blasint* lda, float* s, float complex* u, blasint* ldu, float complex* vt, blasint* ldvt, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz) __attribute__((alias(MTS(FC_GLOBAL(cgesdd,CGESDD)))));
void FC_GLOBAL3(cgesdd,CGESDD)(char* jobz, blasint* m, blasint* n, float complex* a, blasint* lda, float* s, float complex* u, blasint* ldu, float complex* vt, blasint* ldvt, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz) __attribute__((alias(MTS(FC_GLOBAL(cgesdd,CGESDD)))));
#else
void FC_GLOBAL2(cgesdd,CGESDD)(char* jobz, blasint* m, blasint* n, float complex* a, blasint* lda, float* s, float complex* u, blasint* ldu, float complex* vt, blasint* ldvt, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz){ FC_GLOBAL(cgesdd,CGESDD)((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobz); }
void FC_GLOBAL3(cgesdd,CGESDD)(char* jobz, blasint* m, blasint* n, float complex* a, blasint* lda, float* s, float complex* u, blasint* ldu, float complex* vt, blasint* ldvt, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz){ FC_GLOBAL(cgesdd,CGESDD)((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobz); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgesdd_(void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz)
{
    void (*fn) (void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz);

    *(void **) & fn = current_backend->lapack.cgesdd.f77_blas_function;

    fn((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);

    return;
}
#ifndef __APPLE__
void flexiblas_real_cgesdd(void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz) __attribute__((alias("flexiblas_real_cgesdd_")));
#else
void flexiblas_real_cgesdd(void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz){flexiblas_real_cgesdd_((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobz);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgesdd_(void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz)
{
    void (*fn) (void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz);
    void (*fn_hook) (void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz);

    *(void **) &fn      = current_backend->lapack.cgesdd.f77_blas_function;

    hook_pos_cgesdd ++;
    if( hook_pos_cgesdd < __flexiblas_hooks->cgesdd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgesdd.f77_hook_function[hook_pos_cgesdd];
        fn_hook((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);
    } else {
        hook_pos_cgesdd = 0;
        fn((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_cgesdd(void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz) __attribute__((alias("flexiblas_chain_cgesdd_")));
#else
void flexiblas_chain_cgesdd(void* jobz, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* vt, void* ldvt, void* work, void* lwork, void* rwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_jobz){flexiblas_chain_cgesdd_((void*) jobz, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobz);}
#endif



