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


static TLS_STORE uint8_t hook_pos_chseqr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(chseqr,CHSEQR)(char* job, char* compz, blasint* n, blasint* ilo, blasint* ihi, float complex* h, blasint* ldh, float complex* w, float complex* z, blasint* ldz, float complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz)
#else
void FC_GLOBAL(chseqr,CHSEQR)(char* job, char* compz, blasint* n, blasint* ilo, blasint* ihi, float complex* h, blasint* ldh, float complex* w, float complex* z, blasint* ldz, float complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz)
#endif
{
    void (*fn) (void* job, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* w, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz);
    void (*fn_hook) (void* job, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* w, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.chseqr.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->chseqr.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) job, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compz);
        return;
    } else {
        hook_pos_chseqr = 0;
        fn_hook((void*) job, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compz);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(chseqr,CHSEQR)(char* job, char* compz, blasint* n, blasint* ilo, blasint* ihi, float complex* h, blasint* ldh, float complex* w, float complex* z, blasint* ldz, float complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz) __attribute__((alias(MTS(FC_GLOBAL(chseqr,CHSEQR)))));
void FC_GLOBAL3(chseqr,CHSEQR)(char* job, char* compz, blasint* n, blasint* ilo, blasint* ihi, float complex* h, blasint* ldh, float complex* w, float complex* z, blasint* ldz, float complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz) __attribute__((alias(MTS(FC_GLOBAL(chseqr,CHSEQR)))));
#else
void FC_GLOBAL2(chseqr,CHSEQR)(char* job, char* compz, blasint* n, blasint* ilo, blasint* ihi, float complex* h, blasint* ldh, float complex* w, float complex* z, blasint* ldz, float complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz){ FC_GLOBAL(chseqr,CHSEQR)((void*) job, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_compz); }
void FC_GLOBAL3(chseqr,CHSEQR)(char* job, char* compz, blasint* n, blasint* ilo, blasint* ihi, float complex* h, blasint* ldh, float complex* w, float complex* z, blasint* ldz, float complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz){ FC_GLOBAL(chseqr,CHSEQR)((void*) job, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_compz); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_chseqr_(void* job, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* w, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz)
{
    void (*fn) (void* job, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* w, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz);

    *(void **) & fn = current_backend->lapack.chseqr.f77_blas_function;

    fn((void*) job, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compz);

    return;
}
#ifndef __APPLE__
void flexiblas_real_chseqr(void* job, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* w, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz) __attribute__((alias("flexiblas_real_chseqr_")));
#else
void flexiblas_real_chseqr(void* job, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* w, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz){flexiblas_real_chseqr_((void*) job, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_compz);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_chseqr_(void* job, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* w, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz)
{
    void (*fn) (void* job, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* w, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz);
    void (*fn_hook) (void* job, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* w, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz);

    *(void **) &fn      = current_backend->lapack.chseqr.f77_blas_function;

    hook_pos_chseqr ++;
    if( hook_pos_chseqr < __flexiblas_hooks->chseqr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->chseqr.f77_hook_function[hook_pos_chseqr];
        fn_hook((void*) job, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compz);
    } else {
        hook_pos_chseqr = 0;
        fn((void*) job, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compz);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_chseqr(void* job, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* w, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz) __attribute__((alias("flexiblas_chain_chseqr_")));
#else
void flexiblas_chain_chseqr(void* job, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* w, void* z, void* ldz, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz){flexiblas_chain_chseqr_((void*) job, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_compz);}
#endif



