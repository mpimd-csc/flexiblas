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


static TLS_STORE uint8_t hook_pos_ssbgst = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ssbgst,SSBGST)(char* vect, char* uplo, blasint* n, blasint* ka, blasint* kb, float* ab, blasint* ldab, float* bb, blasint* ldbb, float* x, blasint* ldx, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(ssbgst,SSBGST)(char* vect, char* uplo, blasint* n, blasint* ka, blasint* kb, float* ab, blasint* ldab, float* bb, blasint* ldbb, float* x, blasint* ldx, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    void (*fn) (void* vect, void* uplo, void* n, void* ka, void* kb, void* ab, void* ldab, void* bb, void* ldbb, void* x, void* ldx, void* work, void* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* vect, void* uplo, void* n, void* ka, void* kb, void* ab, void* ldab, void* bb, void* ldbb, void* x, void* ldx, void* work, void* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ssbgst.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ssbgst.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) vect, (void*) uplo, (void*) n, (void*) ka, (void*) kb, (void*) ab, (void*) ldab, (void*) bb, (void*) ldbb, (void*) x, (void*) ldx, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    } else {
        hook_pos_ssbgst = 0;
        fn_hook((void*) vect, (void*) uplo, (void*) n, (void*) ka, (void*) kb, (void*) ab, (void*) ldab, (void*) bb, (void*) ldbb, (void*) x, (void*) ldx, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(ssbgst,SSBGST)(char* vect, char* uplo, blasint* n, blasint* ka, blasint* kb, float* ab, blasint* ldab, float* bb, blasint* ldbb, float* x, blasint* ldx, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(ssbgst,SSBGST)))));
void FC_GLOBAL3(ssbgst,SSBGST)(char* vect, char* uplo, blasint* n, blasint* ka, blasint* kb, float* ab, blasint* ldab, float* bb, blasint* ldbb, float* x, blasint* ldx, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(ssbgst,SSBGST)))));
#else
void FC_GLOBAL2(ssbgst,SSBGST)(char* vect, char* uplo, blasint* n, blasint* ka, blasint* kb, float* ab, blasint* ldab, float* bb, blasint* ldbb, float* x, blasint* ldx, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(ssbgst,SSBGST)((void*) vect, (void*) uplo, (void*) n, (void*) ka, (void*) kb, (void*) ab, (void*) ldab, (void*) bb, (void*) ldbb, (void*) x, (void*) ldx, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_vect, (flexiblas_fortran_charlen_t) len_uplo); }
void FC_GLOBAL3(ssbgst,SSBGST)(char* vect, char* uplo, blasint* n, blasint* ka, blasint* kb, float* ab, blasint* ldab, float* bb, blasint* ldbb, float* x, blasint* ldx, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(ssbgst,SSBGST)((void*) vect, (void*) uplo, (void*) n, (void*) ka, (void*) kb, (void*) ab, (void*) ldab, (void*) bb, (void*) ldbb, (void*) x, (void*) ldx, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_vect, (flexiblas_fortran_charlen_t) len_uplo); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ssbgst_(void* vect, void* uplo, void* n, void* ka, void* kb, void* ab, void* ldab, void* bb, void* ldbb, void* x, void* ldx, void* work, void* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* vect, void* uplo, void* n, void* ka, void* kb, void* ab, void* ldab, void* bb, void* ldbb, void* x, void* ldx, void* work, void* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);

    *(void **) & fn = current_backend->lapack.ssbgst.f77_blas_function;

    fn((void*) vect, (void*) uplo, (void*) n, (void*) ka, (void*) kb, (void*) ab, (void*) ldab, (void*) bb, (void*) ldbb, (void*) x, (void*) ldx, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect, ( flexiblas_fortran_charlen_t ) len_uplo);

    return;
}
#ifndef __APPLE__
void flexiblas_real_ssbgst(void* vect, void* uplo, void* n, void* ka, void* kb, void* ab, void* ldab, void* bb, void* ldbb, void* x, void* ldx, void* work, void* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_ssbgst_")));
#else
void flexiblas_real_ssbgst(void* vect, void* uplo, void* n, void* ka, void* kb, void* ab, void* ldab, void* bb, void* ldbb, void* x, void* ldx, void* work, void* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_ssbgst_((void*) vect, (void*) uplo, (void*) n, (void*) ka, (void*) kb, (void*) ab, (void*) ldab, (void*) bb, (void*) ldbb, (void*) x, (void*) ldx, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_vect, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ssbgst_(void* vect, void* uplo, void* n, void* ka, void* kb, void* ab, void* ldab, void* bb, void* ldbb, void* x, void* ldx, void* work, void* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* vect, void* uplo, void* n, void* ka, void* kb, void* ab, void* ldab, void* bb, void* ldbb, void* x, void* ldx, void* work, void* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* vect, void* uplo, void* n, void* ka, void* kb, void* ab, void* ldab, void* bb, void* ldbb, void* x, void* ldx, void* work, void* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);

    *(void **) &fn      = current_backend->lapack.ssbgst.f77_blas_function;

    hook_pos_ssbgst ++;
    if( hook_pos_ssbgst < __flexiblas_hooks->ssbgst.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ssbgst.f77_hook_function[hook_pos_ssbgst];
        fn_hook((void*) vect, (void*) uplo, (void*) n, (void*) ka, (void*) kb, (void*) ab, (void*) ldab, (void*) bb, (void*) ldbb, (void*) x, (void*) ldx, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_ssbgst = 0;
        fn((void*) vect, (void*) uplo, (void*) n, (void*) ka, (void*) kb, (void*) ab, (void*) ldab, (void*) bb, (void*) ldbb, (void*) x, (void*) ldx, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_ssbgst(void* vect, void* uplo, void* n, void* ka, void* kb, void* ab, void* ldab, void* bb, void* ldbb, void* x, void* ldx, void* work, void* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_ssbgst_")));
#else
void flexiblas_chain_ssbgst(void* vect, void* uplo, void* n, void* ka, void* kb, void* ab, void* ldab, void* bb, void* ldbb, void* x, void* ldx, void* work, void* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_ssbgst_((void*) vect, (void*) uplo, (void*) n, (void*) ka, (void*) kb, (void*) ab, (void*) ldab, (void*) bb, (void*) ldbb, (void*) x, (void*) ldx, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_vect, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



