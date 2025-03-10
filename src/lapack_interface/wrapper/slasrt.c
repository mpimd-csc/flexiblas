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


static TLS_STORE uint8_t hook_pos_slasrt = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasrt,SLASRT)(char* id, blasint* n, float* d, blasint* info, flexiblas_fortran_charlen_t len_id)
#else
void FC_GLOBAL(slasrt,SLASRT)(char* id, blasint* n, float* d, blasint* info, flexiblas_fortran_charlen_t len_id)
#endif
{
    void (*fn) (void* id, void* n, void* d, void* info, flexiblas_fortran_charlen_t len_id);
    void (*fn_hook) (void* id, void* n, void* d, void* info, flexiblas_fortran_charlen_t len_id);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slasrt.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slasrt.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) id, (void*) n, (void*) d, (void*) info, ( flexiblas_fortran_charlen_t ) len_id);
        return;
    } else {
        hook_pos_slasrt = 0;
        fn_hook((void*) id, (void*) n, (void*) d, (void*) info, ( flexiblas_fortran_charlen_t ) len_id);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slasrt,SLASRT)(char* id, blasint* n, float* d, blasint* info, flexiblas_fortran_charlen_t len_id) __attribute__((alias(MTS(FC_GLOBAL(slasrt,SLASRT)))));
void FC_GLOBAL3(slasrt,SLASRT)(char* id, blasint* n, float* d, blasint* info, flexiblas_fortran_charlen_t len_id) __attribute__((alias(MTS(FC_GLOBAL(slasrt,SLASRT)))));
#else
void FC_GLOBAL2(slasrt,SLASRT)(char* id, blasint* n, float* d, blasint* info, flexiblas_fortran_charlen_t len_id){ FC_GLOBAL(slasrt,SLASRT)((void*) id, (void*) n, (void*) d, (void*) info, (flexiblas_fortran_charlen_t) len_id); }
void FC_GLOBAL3(slasrt,SLASRT)(char* id, blasint* n, float* d, blasint* info, flexiblas_fortran_charlen_t len_id){ FC_GLOBAL(slasrt,SLASRT)((void*) id, (void*) n, (void*) d, (void*) info, (flexiblas_fortran_charlen_t) len_id); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasrt_(void* id, void* n, void* d, void* info, flexiblas_fortran_charlen_t len_id)
{
    void (*fn) (void* id, void* n, void* d, void* info, flexiblas_fortran_charlen_t len_id);

    *(void **) & fn = current_backend->lapack.slasrt.f77_blas_function;

    fn((void*) id, (void*) n, (void*) d, (void*) info, ( flexiblas_fortran_charlen_t ) len_id);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slasrt(void* id, void* n, void* d, void* info, flexiblas_fortran_charlen_t len_id) __attribute__((alias("flexiblas_real_slasrt_")));
#else
void flexiblas_real_slasrt(void* id, void* n, void* d, void* info, flexiblas_fortran_charlen_t len_id){flexiblas_real_slasrt_((void*) id, (void*) n, (void*) d, (void*) info, (flexiblas_fortran_charlen_t) len_id);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slasrt_(void* id, void* n, void* d, void* info, flexiblas_fortran_charlen_t len_id)
{
    void (*fn) (void* id, void* n, void* d, void* info, flexiblas_fortran_charlen_t len_id);
    void (*fn_hook) (void* id, void* n, void* d, void* info, flexiblas_fortran_charlen_t len_id);

    *(void **) &fn      = current_backend->lapack.slasrt.f77_blas_function;

    hook_pos_slasrt ++;
    if( hook_pos_slasrt < __flexiblas_hooks->slasrt.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slasrt.f77_hook_function[hook_pos_slasrt];
        fn_hook((void*) id, (void*) n, (void*) d, (void*) info, ( flexiblas_fortran_charlen_t ) len_id);
    } else {
        hook_pos_slasrt = 0;
        fn((void*) id, (void*) n, (void*) d, (void*) info, ( flexiblas_fortran_charlen_t ) len_id);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slasrt(void* id, void* n, void* d, void* info, flexiblas_fortran_charlen_t len_id) __attribute__((alias("flexiblas_chain_slasrt_")));
#else
void flexiblas_chain_slasrt(void* id, void* n, void* d, void* info, flexiblas_fortran_charlen_t len_id){flexiblas_chain_slasrt_((void*) id, (void*) n, (void*) d, (void*) info, (flexiblas_fortran_charlen_t) len_id);}
#endif



