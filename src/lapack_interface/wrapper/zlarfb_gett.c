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


static TLS_STORE uint8_t hook_pos_zlarfb_gett = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL_(zlarfb_gett,ZLARFB_GETT)(char* ident, blasint* m, blasint* n, blasint* k, double complex* t, blasint* ldt, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* work, blasint* ldwork, flexiblas_fortran_charlen_t len_ident)
#else
void FC_GLOBAL_(zlarfb_gett,ZLARFB_GETT)(char* ident, blasint* m, blasint* n, blasint* k, double complex* t, blasint* ldt, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* work, blasint* ldwork, flexiblas_fortran_charlen_t len_ident)
#endif
{
    void (*fn) (void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident);
    void (*fn_hook) (void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlarfb_gett.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlarfb_gett.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_ident);
        return;
    } else {
        hook_pos_zlarfb_gett = 0;
        fn_hook((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_ident);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2_(zlarfb_gett,ZLARFB_GETT)(char* ident, blasint* m, blasint* n, blasint* k, double complex* t, blasint* ldt, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* work, blasint* ldwork, flexiblas_fortran_charlen_t len_ident) __attribute__((alias(MTS(FC_GLOBAL_(zlarfb_gett,ZLARFB_GETT)))));
void FC_GLOBAL3_(zlarfb_gett,ZLARFB_GETT)(char* ident, blasint* m, blasint* n, blasint* k, double complex* t, blasint* ldt, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* work, blasint* ldwork, flexiblas_fortran_charlen_t len_ident) __attribute__((alias(MTS(FC_GLOBAL_(zlarfb_gett,ZLARFB_GETT)))));
#else
void FC_GLOBAL2_(zlarfb_gett,ZLARFB_GETT)(char* ident, blasint* m, blasint* n, blasint* k, double complex* t, blasint* ldt, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* work, blasint* ldwork, flexiblas_fortran_charlen_t len_ident){ FC_GLOBAL_(zlarfb_gett,ZLARFB_GETT)((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, (flexiblas_fortran_charlen_t) len_ident); }
void FC_GLOBAL3_(zlarfb_gett,ZLARFB_GETT)(char* ident, blasint* m, blasint* n, blasint* k, double complex* t, blasint* ldt, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* work, blasint* ldwork, flexiblas_fortran_charlen_t len_ident){ FC_GLOBAL_(zlarfb_gett,ZLARFB_GETT)((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, (flexiblas_fortran_charlen_t) len_ident); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlarfb_gett_(void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident)
{
    void (*fn) (void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident);

    *(void **) & fn = current_backend->lapack.zlarfb_gett.f77_blas_function;

    fn((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_ident);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlarfb_gett(void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident) __attribute__((alias("flexiblas_real_zlarfb_gett_")));
#else
void flexiblas_real_zlarfb_gett(void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident){flexiblas_real_zlarfb_gett_((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, (flexiblas_fortran_charlen_t) len_ident);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlarfb_gett_(void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident)
{
    void (*fn) (void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident);
    void (*fn_hook) (void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident);

    *(void **) &fn      = current_backend->lapack.zlarfb_gett.f77_blas_function;

    hook_pos_zlarfb_gett ++;
    if( hook_pos_zlarfb_gett < __flexiblas_hooks->zlarfb_gett.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlarfb_gett.f77_hook_function[hook_pos_zlarfb_gett];
        fn_hook((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_ident);
    } else {
        hook_pos_zlarfb_gett = 0;
        fn((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_ident);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlarfb_gett(void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident) __attribute__((alias("flexiblas_chain_zlarfb_gett_")));
#else
void flexiblas_chain_zlarfb_gett(void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident){flexiblas_chain_zlarfb_gett_((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, (flexiblas_fortran_charlen_t) len_ident);}
#endif



