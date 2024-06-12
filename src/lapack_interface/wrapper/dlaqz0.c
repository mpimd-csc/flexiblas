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


static TLS_STORE uint8_t hook_pos_dlaqz0 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlaqz0,DLAQZ0)(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, double* work, blasint* lwork, blasint* rec, blasint* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz)
#else
void FC_GLOBAL(dlaqz0,DLAQZ0)(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, double* work, blasint* lwork, blasint* rec, blasint* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz)
#endif
{
    void (*fn) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz);
    void (*fn_hook) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlaqz0.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlaqz0.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info, ( flexiblas_fortran_charlen_t ) len_wants, ( flexiblas_fortran_charlen_t ) len_wantq, ( flexiblas_fortran_charlen_t ) len_wantz);
        return;
    } else {
        hook_pos_dlaqz0 = 0;
        fn_hook((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info, ( flexiblas_fortran_charlen_t ) len_wants, ( flexiblas_fortran_charlen_t ) len_wantq, ( flexiblas_fortran_charlen_t ) len_wantz);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dlaqz0_(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, double* work, blasint* lwork, blasint* rec, blasint* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz) __attribute__((alias(MTS(FC_GLOBAL(dlaqz0,DLAQZ0)))));
#else
#ifndef __APPLE__
void dlaqz0(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, double* work, blasint* lwork, blasint* rec, blasint* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz) __attribute__((alias(MTS(FC_GLOBAL(dlaqz0,DLAQZ0)))));
#else
void dlaqz0(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, double* work, blasint* lwork, blasint* rec, blasint* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz){ FC_GLOBAL(dlaqz0,DLAQZ0)((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info, (flexiblas_fortran_charlen_t) len_wants, (flexiblas_fortran_charlen_t) len_wantq, (flexiblas_fortran_charlen_t) len_wantz); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlaqz0_(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz)
{
    void (*fn) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz);

    *(void **) & fn = current_backend->lapack.dlaqz0.f77_blas_function;

    fn((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info, ( flexiblas_fortran_charlen_t ) len_wants, ( flexiblas_fortran_charlen_t ) len_wantq, ( flexiblas_fortran_charlen_t ) len_wantz);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlaqz0(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz) __attribute__((alias("flexiblas_real_dlaqz0_")));
#else
void flexiblas_real_dlaqz0(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz){flexiblas_real_dlaqz0_((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info, (flexiblas_fortran_charlen_t) len_wants, (flexiblas_fortran_charlen_t) len_wantq, (flexiblas_fortran_charlen_t) len_wantz);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlaqz0_(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz)
{
    void (*fn) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz);
    void (*fn_hook) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz);

    *(void **) &fn      = current_backend->lapack.dlaqz0.f77_blas_function;

    hook_pos_dlaqz0 ++;
    if( hook_pos_dlaqz0 < __flexiblas_hooks->dlaqz0.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlaqz0.f77_hook_function[hook_pos_dlaqz0];
        fn_hook((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info, ( flexiblas_fortran_charlen_t ) len_wants, ( flexiblas_fortran_charlen_t ) len_wantq, ( flexiblas_fortran_charlen_t ) len_wantz);
    } else {
        hook_pos_dlaqz0 = 0;
        fn((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info, ( flexiblas_fortran_charlen_t ) len_wants, ( flexiblas_fortran_charlen_t ) len_wantq, ( flexiblas_fortran_charlen_t ) len_wantz);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlaqz0(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz) __attribute__((alias("flexiblas_chain_dlaqz0_")));
#else
void flexiblas_chain_dlaqz0(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info, flexiblas_fortran_charlen_t len_wants, flexiblas_fortran_charlen_t len_wantq, flexiblas_fortran_charlen_t len_wantz){flexiblas_chain_dlaqz0_((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info, (flexiblas_fortran_charlen_t) len_wants, (flexiblas_fortran_charlen_t) len_wantq, (flexiblas_fortran_charlen_t) len_wantz);}
#endif



