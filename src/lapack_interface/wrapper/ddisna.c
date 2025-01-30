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


static TLS_STORE uint8_t hook_pos_ddisna = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ddisna,DDISNA)(char* job, blasint* m, blasint* n, double* d, double* sep, blasint* info, flexiblas_fortran_charlen_t len_job)
#else
void FC_GLOBAL(ddisna,DDISNA)(char* job, blasint* m, blasint* n, double* d, double* sep, blasint* info, flexiblas_fortran_charlen_t len_job)
#endif
{
    void (*fn) (void* job, void* m, void* n, void* d, void* sep, void* info, flexiblas_fortran_charlen_t len_job);
    void (*fn_hook) (void* job, void* m, void* n, void* d, void* sep, void* info, flexiblas_fortran_charlen_t len_job);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ddisna.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ddisna.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) job, (void*) m, (void*) n, (void*) d, (void*) sep, (void*) info, ( flexiblas_fortran_charlen_t ) len_job);
        return;
    } else {
        hook_pos_ddisna = 0;
        fn_hook((void*) job, (void*) m, (void*) n, (void*) d, (void*) sep, (void*) info, ( flexiblas_fortran_charlen_t ) len_job);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(ddisna,DDISNA)(char* job, blasint* m, blasint* n, double* d, double* sep, blasint* info, flexiblas_fortran_charlen_t len_job) __attribute__((alias(MTS(FC_GLOBAL(ddisna,DDISNA)))));
void FC_GLOBAL3(ddisna,DDISNA)(char* job, blasint* m, blasint* n, double* d, double* sep, blasint* info, flexiblas_fortran_charlen_t len_job) __attribute__((alias(MTS(FC_GLOBAL(ddisna,DDISNA)))));
#else
void FC_GLOBAL2(ddisna,DDISNA)(char* job, blasint* m, blasint* n, double* d, double* sep, blasint* info, flexiblas_fortran_charlen_t len_job){ FC_GLOBAL(ddisna,DDISNA)((void*) job, (void*) m, (void*) n, (void*) d, (void*) sep, (void*) info, (flexiblas_fortran_charlen_t) len_job); }
void FC_GLOBAL3(ddisna,DDISNA)(char* job, blasint* m, blasint* n, double* d, double* sep, blasint* info, flexiblas_fortran_charlen_t len_job){ FC_GLOBAL(ddisna,DDISNA)((void*) job, (void*) m, (void*) n, (void*) d, (void*) sep, (void*) info, (flexiblas_fortran_charlen_t) len_job); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ddisna_(void* job, void* m, void* n, void* d, void* sep, void* info, flexiblas_fortran_charlen_t len_job)
{
    void (*fn) (void* job, void* m, void* n, void* d, void* sep, void* info, flexiblas_fortran_charlen_t len_job);

    *(void **) & fn = current_backend->lapack.ddisna.f77_blas_function;

    fn((void*) job, (void*) m, (void*) n, (void*) d, (void*) sep, (void*) info, ( flexiblas_fortran_charlen_t ) len_job);

    return;
}
#ifndef __APPLE__
void flexiblas_real_ddisna(void* job, void* m, void* n, void* d, void* sep, void* info, flexiblas_fortran_charlen_t len_job) __attribute__((alias("flexiblas_real_ddisna_")));
#else
void flexiblas_real_ddisna(void* job, void* m, void* n, void* d, void* sep, void* info, flexiblas_fortran_charlen_t len_job){flexiblas_real_ddisna_((void*) job, (void*) m, (void*) n, (void*) d, (void*) sep, (void*) info, (flexiblas_fortran_charlen_t) len_job);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ddisna_(void* job, void* m, void* n, void* d, void* sep, void* info, flexiblas_fortran_charlen_t len_job)
{
    void (*fn) (void* job, void* m, void* n, void* d, void* sep, void* info, flexiblas_fortran_charlen_t len_job);
    void (*fn_hook) (void* job, void* m, void* n, void* d, void* sep, void* info, flexiblas_fortran_charlen_t len_job);

    *(void **) &fn      = current_backend->lapack.ddisna.f77_blas_function;

    hook_pos_ddisna ++;
    if( hook_pos_ddisna < __flexiblas_hooks->ddisna.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ddisna.f77_hook_function[hook_pos_ddisna];
        fn_hook((void*) job, (void*) m, (void*) n, (void*) d, (void*) sep, (void*) info, ( flexiblas_fortran_charlen_t ) len_job);
    } else {
        hook_pos_ddisna = 0;
        fn((void*) job, (void*) m, (void*) n, (void*) d, (void*) sep, (void*) info, ( flexiblas_fortran_charlen_t ) len_job);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_ddisna(void* job, void* m, void* n, void* d, void* sep, void* info, flexiblas_fortran_charlen_t len_job) __attribute__((alias("flexiblas_chain_ddisna_")));
#else
void flexiblas_chain_ddisna(void* job, void* m, void* n, void* d, void* sep, void* info, flexiblas_fortran_charlen_t len_job){flexiblas_chain_ddisna_((void*) job, (void*) m, (void*) n, (void*) d, (void*) sep, (void*) info, (flexiblas_fortran_charlen_t) len_job);}
#endif



