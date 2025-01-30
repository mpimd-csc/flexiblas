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


static TLS_STORE uint8_t hook_pos_zlasr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlasr,ZLASR)(char* side, char* pivot, char* direct, blasint* m, blasint* n, double* c, double* s, double complex* a, blasint* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct)
#else
void FC_GLOBAL(zlasr,ZLASR)(char* side, char* pivot, char* direct, blasint* m, blasint* n, double* c, double* s, double complex* a, blasint* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct)
#endif
{
    void (*fn) (void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct);
    void (*fn_hook) (void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlasr.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlasr.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_pivot, ( flexiblas_fortran_charlen_t ) len_direct);
        return;
    } else {
        hook_pos_zlasr = 0;
        fn_hook((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_pivot, ( flexiblas_fortran_charlen_t ) len_direct);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zlasr,ZLASR)(char* side, char* pivot, char* direct, blasint* m, blasint* n, double* c, double* s, double complex* a, blasint* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct) __attribute__((alias(MTS(FC_GLOBAL(zlasr,ZLASR)))));
void FC_GLOBAL3(zlasr,ZLASR)(char* side, char* pivot, char* direct, blasint* m, blasint* n, double* c, double* s, double complex* a, blasint* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct) __attribute__((alias(MTS(FC_GLOBAL(zlasr,ZLASR)))));
#else
void FC_GLOBAL2(zlasr,ZLASR)(char* side, char* pivot, char* direct, blasint* m, blasint* n, double* c, double* s, double complex* a, blasint* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct){ FC_GLOBAL(zlasr,ZLASR)((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_pivot, (flexiblas_fortran_charlen_t) len_direct); }
void FC_GLOBAL3(zlasr,ZLASR)(char* side, char* pivot, char* direct, blasint* m, blasint* n, double* c, double* s, double complex* a, blasint* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct){ FC_GLOBAL(zlasr,ZLASR)((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_pivot, (flexiblas_fortran_charlen_t) len_direct); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlasr_(void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct)
{
    void (*fn) (void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct);

    *(void **) & fn = current_backend->lapack.zlasr.f77_blas_function;

    fn((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_pivot, ( flexiblas_fortran_charlen_t ) len_direct);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlasr(void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct) __attribute__((alias("flexiblas_real_zlasr_")));
#else
void flexiblas_real_zlasr(void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct){flexiblas_real_zlasr_((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_pivot, (flexiblas_fortran_charlen_t) len_direct);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlasr_(void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct)
{
    void (*fn) (void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct);
    void (*fn_hook) (void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct);

    *(void **) &fn      = current_backend->lapack.zlasr.f77_blas_function;

    hook_pos_zlasr ++;
    if( hook_pos_zlasr < __flexiblas_hooks->zlasr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlasr.f77_hook_function[hook_pos_zlasr];
        fn_hook((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_pivot, ( flexiblas_fortran_charlen_t ) len_direct);
    } else {
        hook_pos_zlasr = 0;
        fn((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_pivot, ( flexiblas_fortran_charlen_t ) len_direct);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlasr(void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct) __attribute__((alias("flexiblas_chain_zlasr_")));
#else
void flexiblas_chain_zlasr(void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct){flexiblas_chain_zlasr_((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_pivot, (flexiblas_fortran_charlen_t) len_direct);}
#endif



