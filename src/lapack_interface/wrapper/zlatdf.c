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


static TLS_STORE uint8_t hook_pos_zlatdf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlatdf,ZLATDF)(blasint* ijob, blasint* n, double complex* z, blasint* ldz, double complex* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv)
#else
void FC_GLOBAL(zlatdf,ZLATDF)(blasint* ijob, blasint* n, double complex* z, blasint* ldz, double complex* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv)
#endif
{
    void (*fn) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);
    void (*fn_hook) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlatdf.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlatdf.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv);
        return;
    } else {
        hook_pos_zlatdf = 0;
        fn_hook((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zlatdf,ZLATDF)(blasint* ijob, blasint* n, double complex* z, blasint* ldz, double complex* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv) __attribute__((alias(MTS(FC_GLOBAL(zlatdf,ZLATDF)))));
void FC_GLOBAL3(zlatdf,ZLATDF)(blasint* ijob, blasint* n, double complex* z, blasint* ldz, double complex* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv) __attribute__((alias(MTS(FC_GLOBAL(zlatdf,ZLATDF)))));
#else
void FC_GLOBAL2(zlatdf,ZLATDF)(blasint* ijob, blasint* n, double complex* z, blasint* ldz, double complex* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv){ FC_GLOBAL(zlatdf,ZLATDF)((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv); }
void FC_GLOBAL3(zlatdf,ZLATDF)(blasint* ijob, blasint* n, double complex* z, blasint* ldz, double complex* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv){ FC_GLOBAL(zlatdf,ZLATDF)((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlatdf_(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv)
{
    void (*fn) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);

    *(void **) & fn = current_backend->lapack.zlatdf.f77_blas_function;

    fn((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlatdf(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv) __attribute__((alias("flexiblas_real_zlatdf_")));
#else
void flexiblas_real_zlatdf(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv){flexiblas_real_zlatdf_((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlatdf_(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv)
{
    void (*fn) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);
    void (*fn_hook) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);

    *(void **) &fn      = current_backend->lapack.zlatdf.f77_blas_function;

    hook_pos_zlatdf ++;
    if( hook_pos_zlatdf < __flexiblas_hooks->zlatdf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlatdf.f77_hook_function[hook_pos_zlatdf];
        fn_hook((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv);
    } else {
        hook_pos_zlatdf = 0;
        fn((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlatdf(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv) __attribute__((alias("flexiblas_chain_zlatdf_")));
#else
void flexiblas_chain_zlatdf(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv){flexiblas_chain_zlatdf_((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv);}
#endif



