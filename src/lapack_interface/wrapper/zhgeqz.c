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


static TLS_STORE uint8_t hook_pos_zhgeqz = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zhgeqz,ZHGEQZ)(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double complex* h, blasint* ldh, double complex* t, blasint* ldt, double complex* alpha, double complex* beta, double complex* q, blasint* ldq, double complex* z, blasint* ldz, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz)
#else
void FC_GLOBAL(zhgeqz,ZHGEQZ)(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double complex* h, blasint* ldh, double complex* t, blasint* ldt, double complex* alpha, double complex* beta, double complex* q, blasint* ldq, double complex* z, blasint* ldz, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz)
#endif
{
    void (*fn) (void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);
    void (*fn_hook) (void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zhgeqz.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zhgeqz.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compq, ( flexiblas_fortran_charlen_t ) len_compz);
        return;
    } else {
        hook_pos_zhgeqz = 0;
        fn_hook((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compq, ( flexiblas_fortran_charlen_t ) len_compz);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zhgeqz,ZHGEQZ)(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double complex* h, blasint* ldh, double complex* t, blasint* ldt, double complex* alpha, double complex* beta, double complex* q, blasint* ldq, double complex* z, blasint* ldz, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz) __attribute__((alias(MTS(FC_GLOBAL(zhgeqz,ZHGEQZ)))));
void FC_GLOBAL3(zhgeqz,ZHGEQZ)(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double complex* h, blasint* ldh, double complex* t, blasint* ldt, double complex* alpha, double complex* beta, double complex* q, blasint* ldq, double complex* z, blasint* ldz, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz) __attribute__((alias(MTS(FC_GLOBAL(zhgeqz,ZHGEQZ)))));
#else
void FC_GLOBAL2(zhgeqz,ZHGEQZ)(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double complex* h, blasint* ldh, double complex* t, blasint* ldt, double complex* alpha, double complex* beta, double complex* q, blasint* ldq, double complex* z, blasint* ldz, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz){ FC_GLOBAL(zhgeqz,ZHGEQZ)((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_compq, (flexiblas_fortran_charlen_t) len_compz); }
void FC_GLOBAL3(zhgeqz,ZHGEQZ)(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double complex* h, blasint* ldh, double complex* t, blasint* ldt, double complex* alpha, double complex* beta, double complex* q, blasint* ldq, double complex* z, blasint* ldz, double complex* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz){ FC_GLOBAL(zhgeqz,ZHGEQZ)((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_compq, (flexiblas_fortran_charlen_t) len_compz); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zhgeqz_(void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz)
{
    void (*fn) (void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

    *(void **) & fn = current_backend->lapack.zhgeqz.f77_blas_function;

    fn((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compq, ( flexiblas_fortran_charlen_t ) len_compz);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zhgeqz(void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz) __attribute__((alias("flexiblas_real_zhgeqz_")));
#else
void flexiblas_real_zhgeqz(void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz){flexiblas_real_zhgeqz_((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_compq, (flexiblas_fortran_charlen_t) len_compz);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zhgeqz_(void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz)
{
    void (*fn) (void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);
    void (*fn_hook) (void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

    *(void **) &fn      = current_backend->lapack.zhgeqz.f77_blas_function;

    hook_pos_zhgeqz ++;
    if( hook_pos_zhgeqz < __flexiblas_hooks->zhgeqz.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zhgeqz.f77_hook_function[hook_pos_zhgeqz];
        fn_hook((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compq, ( flexiblas_fortran_charlen_t ) len_compz);
    } else {
        hook_pos_zhgeqz = 0;
        fn((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compq, ( flexiblas_fortran_charlen_t ) len_compz);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zhgeqz(void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz) __attribute__((alias("flexiblas_chain_zhgeqz_")));
#else
void flexiblas_chain_zhgeqz(void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz){flexiblas_chain_zhgeqz_((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_compq, (flexiblas_fortran_charlen_t) len_compz);}
#endif



