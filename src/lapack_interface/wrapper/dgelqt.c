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


static TLS_STORE uint8_t hook_pos_dgelqt = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dgelqt,DGELQT)(blasint* m, blasint* n, blasint* mb, double* a, blasint* lda, double* t, blasint* ldt, double* work, blasint* info)
#else
void FC_GLOBAL(dgelqt,DGELQT)(blasint* m, blasint* n, blasint* mb, double* a, blasint* lda, double* t, blasint* ldt, double* work, blasint* info)
#endif
{
    void (*fn) (void* m, void* n, void* mb, void* a, void* lda, void* t, void* ldt, void* work, void* info);
    void (*fn_hook) (void* m, void* n, void* mb, void* a, void* lda, void* t, void* ldt, void* work, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dgelqt.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dgelqt.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) m, (void*) n, (void*) mb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) info);
        return;
    } else {
        hook_pos_dgelqt = 0;
        fn_hook((void*) m, (void*) n, (void*) mb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dgelqt,DGELQT)(blasint* m, blasint* n, blasint* mb, double* a, blasint* lda, double* t, blasint* ldt, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dgelqt,DGELQT)))));
void FC_GLOBAL3(dgelqt,DGELQT)(blasint* m, blasint* n, blasint* mb, double* a, blasint* lda, double* t, blasint* ldt, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dgelqt,DGELQT)))));
#else
void FC_GLOBAL2(dgelqt,DGELQT)(blasint* m, blasint* n, blasint* mb, double* a, blasint* lda, double* t, blasint* ldt, double* work, blasint* info){ FC_GLOBAL(dgelqt,DGELQT)((void*) m, (void*) n, (void*) mb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) info); }
void FC_GLOBAL3(dgelqt,DGELQT)(blasint* m, blasint* n, blasint* mb, double* a, blasint* lda, double* t, blasint* ldt, double* work, blasint* info){ FC_GLOBAL(dgelqt,DGELQT)((void*) m, (void*) n, (void*) mb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dgelqt_(void* m, void* n, void* mb, void* a, void* lda, void* t, void* ldt, void* work, void* info)
{
    void (*fn) (void* m, void* n, void* mb, void* a, void* lda, void* t, void* ldt, void* work, void* info);

    *(void **) & fn = current_backend->lapack.dgelqt.f77_blas_function;

    fn((void*) m, (void*) n, (void*) mb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dgelqt(void* m, void* n, void* mb, void* a, void* lda, void* t, void* ldt, void* work, void* info) __attribute__((alias("flexiblas_real_dgelqt_")));
#else
void flexiblas_real_dgelqt(void* m, void* n, void* mb, void* a, void* lda, void* t, void* ldt, void* work, void* info){flexiblas_real_dgelqt_((void*) m, (void*) n, (void*) mb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dgelqt_(void* m, void* n, void* mb, void* a, void* lda, void* t, void* ldt, void* work, void* info)
{
    void (*fn) (void* m, void* n, void* mb, void* a, void* lda, void* t, void* ldt, void* work, void* info);
    void (*fn_hook) (void* m, void* n, void* mb, void* a, void* lda, void* t, void* ldt, void* work, void* info);

    *(void **) &fn      = current_backend->lapack.dgelqt.f77_blas_function;

    hook_pos_dgelqt ++;
    if( hook_pos_dgelqt < __flexiblas_hooks->dgelqt.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dgelqt.f77_hook_function[hook_pos_dgelqt];
        fn_hook((void*) m, (void*) n, (void*) mb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) info);
    } else {
        hook_pos_dgelqt = 0;
        fn((void*) m, (void*) n, (void*) mb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dgelqt(void* m, void* n, void* mb, void* a, void* lda, void* t, void* ldt, void* work, void* info) __attribute__((alias("flexiblas_chain_dgelqt_")));
#else
void flexiblas_chain_dgelqt(void* m, void* n, void* mb, void* a, void* lda, void* t, void* ldt, void* work, void* info){flexiblas_chain_dgelqt_((void*) m, (void*) n, (void*) mb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) info);}
#endif



