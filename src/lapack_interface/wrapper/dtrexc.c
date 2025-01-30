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


static TLS_STORE uint8_t hook_pos_dtrexc = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dtrexc,DTREXC)(char* compq, blasint* n, double* t, blasint* ldt, double* q, blasint* ldq, blasint* ifst, blasint* ilst, double* work, blasint* info, flexiblas_fortran_charlen_t len_compq)
#else
void FC_GLOBAL(dtrexc,DTREXC)(char* compq, blasint* n, double* t, blasint* ldt, double* q, blasint* ldq, blasint* ifst, blasint* ilst, double* work, blasint* info, flexiblas_fortran_charlen_t len_compq)
#endif
{
    void (*fn) (void* compq, void* n, void* t, void* ldt, void* q, void* ldq, void* ifst, void* ilst, void* work, void* info, flexiblas_fortran_charlen_t len_compq);
    void (*fn_hook) (void* compq, void* n, void* t, void* ldt, void* q, void* ldq, void* ifst, void* ilst, void* work, void* info, flexiblas_fortran_charlen_t len_compq);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dtrexc.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dtrexc.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) compq, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) ifst, (void*) ilst, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compq);
        return;
    } else {
        hook_pos_dtrexc = 0;
        fn_hook((void*) compq, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) ifst, (void*) ilst, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compq);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dtrexc,DTREXC)(char* compq, blasint* n, double* t, blasint* ldt, double* q, blasint* ldq, blasint* ifst, blasint* ilst, double* work, blasint* info, flexiblas_fortran_charlen_t len_compq) __attribute__((alias(MTS(FC_GLOBAL(dtrexc,DTREXC)))));
void FC_GLOBAL3(dtrexc,DTREXC)(char* compq, blasint* n, double* t, blasint* ldt, double* q, blasint* ldq, blasint* ifst, blasint* ilst, double* work, blasint* info, flexiblas_fortran_charlen_t len_compq) __attribute__((alias(MTS(FC_GLOBAL(dtrexc,DTREXC)))));
#else
void FC_GLOBAL2(dtrexc,DTREXC)(char* compq, blasint* n, double* t, blasint* ldt, double* q, blasint* ldq, blasint* ifst, blasint* ilst, double* work, blasint* info, flexiblas_fortran_charlen_t len_compq){ FC_GLOBAL(dtrexc,DTREXC)((void*) compq, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) ifst, (void*) ilst, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_compq); }
void FC_GLOBAL3(dtrexc,DTREXC)(char* compq, blasint* n, double* t, blasint* ldt, double* q, blasint* ldq, blasint* ifst, blasint* ilst, double* work, blasint* info, flexiblas_fortran_charlen_t len_compq){ FC_GLOBAL(dtrexc,DTREXC)((void*) compq, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) ifst, (void*) ilst, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_compq); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dtrexc_(void* compq, void* n, void* t, void* ldt, void* q, void* ldq, void* ifst, void* ilst, void* work, void* info, flexiblas_fortran_charlen_t len_compq)
{
    void (*fn) (void* compq, void* n, void* t, void* ldt, void* q, void* ldq, void* ifst, void* ilst, void* work, void* info, flexiblas_fortran_charlen_t len_compq);

    *(void **) & fn = current_backend->lapack.dtrexc.f77_blas_function;

    fn((void*) compq, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) ifst, (void*) ilst, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compq);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dtrexc(void* compq, void* n, void* t, void* ldt, void* q, void* ldq, void* ifst, void* ilst, void* work, void* info, flexiblas_fortran_charlen_t len_compq) __attribute__((alias("flexiblas_real_dtrexc_")));
#else
void flexiblas_real_dtrexc(void* compq, void* n, void* t, void* ldt, void* q, void* ldq, void* ifst, void* ilst, void* work, void* info, flexiblas_fortran_charlen_t len_compq){flexiblas_real_dtrexc_((void*) compq, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) ifst, (void*) ilst, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_compq);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dtrexc_(void* compq, void* n, void* t, void* ldt, void* q, void* ldq, void* ifst, void* ilst, void* work, void* info, flexiblas_fortran_charlen_t len_compq)
{
    void (*fn) (void* compq, void* n, void* t, void* ldt, void* q, void* ldq, void* ifst, void* ilst, void* work, void* info, flexiblas_fortran_charlen_t len_compq);
    void (*fn_hook) (void* compq, void* n, void* t, void* ldt, void* q, void* ldq, void* ifst, void* ilst, void* work, void* info, flexiblas_fortran_charlen_t len_compq);

    *(void **) &fn      = current_backend->lapack.dtrexc.f77_blas_function;

    hook_pos_dtrexc ++;
    if( hook_pos_dtrexc < __flexiblas_hooks->dtrexc.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dtrexc.f77_hook_function[hook_pos_dtrexc];
        fn_hook((void*) compq, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) ifst, (void*) ilst, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compq);
    } else {
        hook_pos_dtrexc = 0;
        fn((void*) compq, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) ifst, (void*) ilst, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_compq);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dtrexc(void* compq, void* n, void* t, void* ldt, void* q, void* ldq, void* ifst, void* ilst, void* work, void* info, flexiblas_fortran_charlen_t len_compq) __attribute__((alias("flexiblas_chain_dtrexc_")));
#else
void flexiblas_chain_dtrexc(void* compq, void* n, void* t, void* ldt, void* q, void* ldq, void* ifst, void* ilst, void* work, void* info, flexiblas_fortran_charlen_t len_compq){flexiblas_chain_dtrexc_((void*) compq, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) ifst, (void*) ilst, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_compq);}
#endif



