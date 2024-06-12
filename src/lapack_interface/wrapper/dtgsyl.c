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


static TLS_STORE uint8_t hook_pos_dtgsyl = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dtgsyl,DTGSYL)(char* trans, blasint* ijob, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* c, blasint* ldc, double* d, blasint* ldd, double* e, blasint* lde, double* f, blasint* ldf, double* scale, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans)
#else
void FC_GLOBAL(dtgsyl,DTGSYL)(char* trans, blasint* ijob, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* c, blasint* ldc, double* d, blasint* ldd, double* e, blasint* lde, double* f, blasint* ldf, double* scale, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans)
#endif
{
    void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_trans);
    void (*fn_hook) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_trans);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dtgsyl.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dtgsyl.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans);
        return;
    } else {
        hook_pos_dtgsyl = 0;
        fn_hook((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dtgsyl_(char* trans, blasint* ijob, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* c, blasint* ldc, double* d, blasint* ldd, double* e, blasint* lde, double* f, blasint* ldf, double* scale, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(dtgsyl,DTGSYL)))));
#else
#ifndef __APPLE__
void dtgsyl(char* trans, blasint* ijob, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* c, blasint* ldc, double* d, blasint* ldd, double* e, blasint* lde, double* f, blasint* ldf, double* scale, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(dtgsyl,DTGSYL)))));
#else
void dtgsyl(char* trans, blasint* ijob, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* c, blasint* ldc, double* d, blasint* ldd, double* e, blasint* lde, double* f, blasint* ldf, double* scale, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(dtgsyl,DTGSYL)((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_trans); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dtgsyl_(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_trans)
{
    void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_trans);

    *(void **) & fn = current_backend->lapack.dtgsyl.f77_blas_function;

    fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dtgsyl(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_dtgsyl_")));
#else
void flexiblas_real_dtgsyl(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_trans){flexiblas_real_dtgsyl_((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dtgsyl_(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_trans)
{
    void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_trans);
    void (*fn_hook) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_trans);

    *(void **) &fn      = current_backend->lapack.dtgsyl.f77_blas_function;

    hook_pos_dtgsyl ++;
    if( hook_pos_dtgsyl < __flexiblas_hooks->dtgsyl.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dtgsyl.f77_hook_function[hook_pos_dtgsyl];
        fn_hook((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans);
    } else {
        hook_pos_dtgsyl = 0;
        fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dtgsyl(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_dtgsyl_")));
#else
void flexiblas_chain_dtgsyl(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_trans){flexiblas_chain_dtgsyl_((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_trans);}
#endif



