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


static TLS_STORE uint8_t hook_pos_sopgtr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sopgtr,SOPGTR)(char* uplo, blasint* n, float* ap, float* tau, float* q, blasint* ldq, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(sopgtr,SOPGTR)(char* uplo, blasint* n, float* ap, float* tau, float* q, blasint* ldq, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    void (*fn) (void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.sopgtr.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->sopgtr.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    } else {
        hook_pos_sopgtr = 0;
        fn_hook((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(sopgtr,SOPGTR)(char* uplo, blasint* n, float* ap, float* tau, float* q, blasint* ldq, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(sopgtr,SOPGTR)))));
void FC_GLOBAL3(sopgtr,SOPGTR)(char* uplo, blasint* n, float* ap, float* tau, float* q, blasint* ldq, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(sopgtr,SOPGTR)))));
#else
void FC_GLOBAL2(sopgtr,SOPGTR)(char* uplo, blasint* n, float* ap, float* tau, float* q, blasint* ldq, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(sopgtr,SOPGTR)((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
void FC_GLOBAL3(sopgtr,SOPGTR)(char* uplo, blasint* n, float* ap, float* tau, float* q, blasint* ldq, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(sopgtr,SOPGTR)((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sopgtr_(void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) & fn = current_backend->lapack.sopgtr.f77_blas_function;

    fn((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);

    return;
}
#ifndef __APPLE__
void flexiblas_real_sopgtr(void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_sopgtr_")));
#else
void flexiblas_real_sopgtr(void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_sopgtr_((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sopgtr_(void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) &fn      = current_backend->lapack.sopgtr.f77_blas_function;

    hook_pos_sopgtr ++;
    if( hook_pos_sopgtr < __flexiblas_hooks->sopgtr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sopgtr.f77_hook_function[hook_pos_sopgtr];
        fn_hook((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_sopgtr = 0;
        fn((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_sopgtr(void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_sopgtr_")));
#else
void flexiblas_chain_sopgtr(void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_sopgtr_((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



