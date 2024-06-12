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


static TLS_STORE uint8_t hook_pos_dlasdt = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlasdt,DLASDT)(blasint* n, blasint* lvl, blasint* nd, blasint* inode, blasint* ndiml, blasint* ndimr, blasint* msub)
#else
void FC_GLOBAL(dlasdt,DLASDT)(blasint* n, blasint* lvl, blasint* nd, blasint* inode, blasint* ndiml, blasint* ndimr, blasint* msub)
#endif
{
    void (*fn) (void* n, void* lvl, void* nd, void* inode, void* ndiml, void* ndimr, void* msub);
    void (*fn_hook) (void* n, void* lvl, void* nd, void* inode, void* ndiml, void* ndimr, void* msub);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlasdt.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlasdt.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) lvl, (void*) nd, (void*) inode, (void*) ndiml, (void*) ndimr, (void*) msub);
        return;
    } else {
        hook_pos_dlasdt = 0;
        fn_hook((void*) n, (void*) lvl, (void*) nd, (void*) inode, (void*) ndiml, (void*) ndimr, (void*) msub);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dlasdt_(blasint* n, blasint* lvl, blasint* nd, blasint* inode, blasint* ndiml, blasint* ndimr, blasint* msub) __attribute__((alias(MTS(FC_GLOBAL(dlasdt,DLASDT)))));
#else
#ifndef __APPLE__
void dlasdt(blasint* n, blasint* lvl, blasint* nd, blasint* inode, blasint* ndiml, blasint* ndimr, blasint* msub) __attribute__((alias(MTS(FC_GLOBAL(dlasdt,DLASDT)))));
#else
void dlasdt(blasint* n, blasint* lvl, blasint* nd, blasint* inode, blasint* ndiml, blasint* ndimr, blasint* msub){ FC_GLOBAL(dlasdt,DLASDT)((void*) n, (void*) lvl, (void*) nd, (void*) inode, (void*) ndiml, (void*) ndimr, (void*) msub); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlasdt_(void* n, void* lvl, void* nd, void* inode, void* ndiml, void* ndimr, void* msub)
{
    void (*fn) (void* n, void* lvl, void* nd, void* inode, void* ndiml, void* ndimr, void* msub);

    *(void **) & fn = current_backend->lapack.dlasdt.f77_blas_function;

    fn((void*) n, (void*) lvl, (void*) nd, (void*) inode, (void*) ndiml, (void*) ndimr, (void*) msub);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlasdt(void* n, void* lvl, void* nd, void* inode, void* ndiml, void* ndimr, void* msub) __attribute__((alias("flexiblas_real_dlasdt_")));
#else
void flexiblas_real_dlasdt(void* n, void* lvl, void* nd, void* inode, void* ndiml, void* ndimr, void* msub){flexiblas_real_dlasdt_((void*) n, (void*) lvl, (void*) nd, (void*) inode, (void*) ndiml, (void*) ndimr, (void*) msub);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlasdt_(void* n, void* lvl, void* nd, void* inode, void* ndiml, void* ndimr, void* msub)
{
    void (*fn) (void* n, void* lvl, void* nd, void* inode, void* ndiml, void* ndimr, void* msub);
    void (*fn_hook) (void* n, void* lvl, void* nd, void* inode, void* ndiml, void* ndimr, void* msub);

    *(void **) &fn      = current_backend->lapack.dlasdt.f77_blas_function;

    hook_pos_dlasdt ++;
    if( hook_pos_dlasdt < __flexiblas_hooks->dlasdt.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlasdt.f77_hook_function[hook_pos_dlasdt];
        fn_hook((void*) n, (void*) lvl, (void*) nd, (void*) inode, (void*) ndiml, (void*) ndimr, (void*) msub);
    } else {
        hook_pos_dlasdt = 0;
        fn((void*) n, (void*) lvl, (void*) nd, (void*) inode, (void*) ndiml, (void*) ndimr, (void*) msub);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlasdt(void* n, void* lvl, void* nd, void* inode, void* ndiml, void* ndimr, void* msub) __attribute__((alias("flexiblas_chain_dlasdt_")));
#else
void flexiblas_chain_dlasdt(void* n, void* lvl, void* nd, void* inode, void* ndiml, void* ndimr, void* msub){flexiblas_chain_dlasdt_((void*) n, (void*) lvl, (void*) nd, (void*) inode, (void*) ndiml, (void*) ndimr, (void*) msub);}
#endif



