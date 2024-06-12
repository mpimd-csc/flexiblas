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


static TLS_STORE uint8_t hook_pos_cgbsvx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgbsvx,CGBSVX)(char* fact, char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float complex* ab, blasint* ldab, float complex* afb, blasint* ldafb, blasint* ipiv, char* equed, float* r, float* c, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* rcond, float* ferr, float* berr, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed)
#else
void FC_GLOBAL(cgbsvx,CGBSVX)(char* fact, char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float complex* ab, blasint* ldab, float complex* afb, blasint* ldafb, blasint* ipiv, char* equed, float* r, float* c, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* rcond, float* ferr, float* berr, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed)
#endif
{
    void (*fn) (void* fact, void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);
    void (*fn_hook) (void* fact, void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.cgbsvx.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->cgbsvx.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) fact, (void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_equed);
        return;
    } else {
        hook_pos_cgbsvx = 0;
        fn_hook((void*) fact, (void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_equed);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void cgbsvx_(char* fact, char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float complex* ab, blasint* ldab, float complex* afb, blasint* ldafb, blasint* ipiv, char* equed, float* r, float* c, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* rcond, float* ferr, float* berr, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed) __attribute__((alias(MTS(FC_GLOBAL(cgbsvx,CGBSVX)))));
#else
#ifndef __APPLE__
void cgbsvx(char* fact, char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float complex* ab, blasint* ldab, float complex* afb, blasint* ldafb, blasint* ipiv, char* equed, float* r, float* c, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* rcond, float* ferr, float* berr, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed) __attribute__((alias(MTS(FC_GLOBAL(cgbsvx,CGBSVX)))));
#else
void cgbsvx(char* fact, char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float complex* ab, blasint* ldab, float complex* afb, blasint* ldafb, blasint* ipiv, char* equed, float* r, float* c, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* rcond, float* ferr, float* berr, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed){ FC_GLOBAL(cgbsvx,CGBSVX)((void*) fact, (void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_fact, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_equed); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgbsvx_(void* fact, void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed)
{
    void (*fn) (void* fact, void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

    *(void **) & fn = current_backend->lapack.cgbsvx.f77_blas_function;

    fn((void*) fact, (void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_equed);

    return;
}
#ifndef __APPLE__
void flexiblas_real_cgbsvx(void* fact, void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed) __attribute__((alias("flexiblas_real_cgbsvx_")));
#else
void flexiblas_real_cgbsvx(void* fact, void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed){flexiblas_real_cgbsvx_((void*) fact, (void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_fact, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_equed);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgbsvx_(void* fact, void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed)
{
    void (*fn) (void* fact, void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);
    void (*fn_hook) (void* fact, void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

    *(void **) &fn      = current_backend->lapack.cgbsvx.f77_blas_function;

    hook_pos_cgbsvx ++;
    if( hook_pos_cgbsvx < __flexiblas_hooks->cgbsvx.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgbsvx.f77_hook_function[hook_pos_cgbsvx];
        fn_hook((void*) fact, (void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_equed);
    } else {
        hook_pos_cgbsvx = 0;
        fn((void*) fact, (void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_fact, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_equed);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_cgbsvx(void* fact, void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed) __attribute__((alias("flexiblas_chain_cgbsvx_")));
#else
void flexiblas_chain_cgbsvx(void* fact, void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* ipiv, void* equed, void* r, void* c, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed){flexiblas_chain_cgbsvx_((void*) fact, (void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) ipiv, (void*) equed, (void*) r, (void*) c, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_fact, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_equed);}
#endif



