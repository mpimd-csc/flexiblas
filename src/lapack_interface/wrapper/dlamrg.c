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


static TLS_STORE uint8_t hook_pos_dlamrg = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlamrg,DLAMRG)(blasint* n1, blasint* n2, double* a, blasint* dtrd1, blasint* dtrd2, blasint* index_bn)
#else
void FC_GLOBAL(dlamrg,DLAMRG)(blasint* n1, blasint* n2, double* a, blasint* dtrd1, blasint* dtrd2, blasint* index_bn)
#endif
{
    void (*fn) (void* n1, void* n2, void* a, void* dtrd1, void* dtrd2, void* index_bn);
    void (*fn_hook) (void* n1, void* n2, void* a, void* dtrd1, void* dtrd2, void* index_bn);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlamrg.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlamrg.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n1, (void*) n2, (void*) a, (void*) dtrd1, (void*) dtrd2, (void*) index_bn);
        return;
    } else {
        hook_pos_dlamrg = 0;
        fn_hook((void*) n1, (void*) n2, (void*) a, (void*) dtrd1, (void*) dtrd2, (void*) index_bn);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dlamrg_(blasint* n1, blasint* n2, double* a, blasint* dtrd1, blasint* dtrd2, blasint* index_bn) __attribute__((alias(MTS(FC_GLOBAL(dlamrg,DLAMRG)))));
#else
#ifndef __APPLE__
void dlamrg(blasint* n1, blasint* n2, double* a, blasint* dtrd1, blasint* dtrd2, blasint* index_bn) __attribute__((alias(MTS(FC_GLOBAL(dlamrg,DLAMRG)))));
#else
void dlamrg(blasint* n1, blasint* n2, double* a, blasint* dtrd1, blasint* dtrd2, blasint* index_bn){ FC_GLOBAL(dlamrg,DLAMRG)((void*) n1, (void*) n2, (void*) a, (void*) dtrd1, (void*) dtrd2, (void*) index_bn); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlamrg_(void* n1, void* n2, void* a, void* dtrd1, void* dtrd2, void* index_bn)
{
    void (*fn) (void* n1, void* n2, void* a, void* dtrd1, void* dtrd2, void* index_bn);

    *(void **) & fn = current_backend->lapack.dlamrg.f77_blas_function;

    fn((void*) n1, (void*) n2, (void*) a, (void*) dtrd1, (void*) dtrd2, (void*) index_bn);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlamrg(void* n1, void* n2, void* a, void* dtrd1, void* dtrd2, void* index_bn) __attribute__((alias("flexiblas_real_dlamrg_")));
#else
void flexiblas_real_dlamrg(void* n1, void* n2, void* a, void* dtrd1, void* dtrd2, void* index_bn){flexiblas_real_dlamrg_((void*) n1, (void*) n2, (void*) a, (void*) dtrd1, (void*) dtrd2, (void*) index_bn);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlamrg_(void* n1, void* n2, void* a, void* dtrd1, void* dtrd2, void* index_bn)
{
    void (*fn) (void* n1, void* n2, void* a, void* dtrd1, void* dtrd2, void* index_bn);
    void (*fn_hook) (void* n1, void* n2, void* a, void* dtrd1, void* dtrd2, void* index_bn);

    *(void **) &fn      = current_backend->lapack.dlamrg.f77_blas_function;

    hook_pos_dlamrg ++;
    if( hook_pos_dlamrg < __flexiblas_hooks->dlamrg.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlamrg.f77_hook_function[hook_pos_dlamrg];
        fn_hook((void*) n1, (void*) n2, (void*) a, (void*) dtrd1, (void*) dtrd2, (void*) index_bn);
    } else {
        hook_pos_dlamrg = 0;
        fn((void*) n1, (void*) n2, (void*) a, (void*) dtrd1, (void*) dtrd2, (void*) index_bn);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlamrg(void* n1, void* n2, void* a, void* dtrd1, void* dtrd2, void* index_bn) __attribute__((alias("flexiblas_chain_dlamrg_")));
#else
void flexiblas_chain_dlamrg(void* n1, void* n2, void* a, void* dtrd1, void* dtrd2, void* index_bn){flexiblas_chain_dlamrg_((void*) n1, (void*) n2, (void*) a, (void*) dtrd1, (void*) dtrd2, (void*) index_bn);}
#endif



