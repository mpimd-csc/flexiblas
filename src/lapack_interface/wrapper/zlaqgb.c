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


static TLS_STORE uint8_t hook_pos_zlaqgb = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlaqgb,ZLAQGB)(blasint* m, blasint* n, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed, flexiblas_fortran_charlen_t len_equed)
#else
void FC_GLOBAL(zlaqgb,ZLAQGB)(blasint* m, blasint* n, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed, flexiblas_fortran_charlen_t len_equed)
#endif
{
    void (*fn) (void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed, flexiblas_fortran_charlen_t len_equed);
    void (*fn_hook) (void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed, flexiblas_fortran_charlen_t len_equed);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlaqgb.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlaqgb.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed, ( flexiblas_fortran_charlen_t ) len_equed);
        return;
    } else {
        hook_pos_zlaqgb = 0;
        fn_hook((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed, ( flexiblas_fortran_charlen_t ) len_equed);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zlaqgb,ZLAQGB)(blasint* m, blasint* n, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed, flexiblas_fortran_charlen_t len_equed) __attribute__((alias(MTS(FC_GLOBAL(zlaqgb,ZLAQGB)))));
void FC_GLOBAL3(zlaqgb,ZLAQGB)(blasint* m, blasint* n, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed, flexiblas_fortran_charlen_t len_equed) __attribute__((alias(MTS(FC_GLOBAL(zlaqgb,ZLAQGB)))));
#else
void FC_GLOBAL2(zlaqgb,ZLAQGB)(blasint* m, blasint* n, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed, flexiblas_fortran_charlen_t len_equed){ FC_GLOBAL(zlaqgb,ZLAQGB)((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed, (flexiblas_fortran_charlen_t) len_equed); }
void FC_GLOBAL3(zlaqgb,ZLAQGB)(blasint* m, blasint* n, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed, flexiblas_fortran_charlen_t len_equed){ FC_GLOBAL(zlaqgb,ZLAQGB)((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed, (flexiblas_fortran_charlen_t) len_equed); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaqgb_(void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed, flexiblas_fortran_charlen_t len_equed)
{
    void (*fn) (void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed, flexiblas_fortran_charlen_t len_equed);

    *(void **) & fn = current_backend->lapack.zlaqgb.f77_blas_function;

    fn((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed, ( flexiblas_fortran_charlen_t ) len_equed);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlaqgb(void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed, flexiblas_fortran_charlen_t len_equed) __attribute__((alias("flexiblas_real_zlaqgb_")));
#else
void flexiblas_real_zlaqgb(void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed, flexiblas_fortran_charlen_t len_equed){flexiblas_real_zlaqgb_((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed, (flexiblas_fortran_charlen_t) len_equed);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlaqgb_(void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed, flexiblas_fortran_charlen_t len_equed)
{
    void (*fn) (void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed, flexiblas_fortran_charlen_t len_equed);
    void (*fn_hook) (void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed, flexiblas_fortran_charlen_t len_equed);

    *(void **) &fn      = current_backend->lapack.zlaqgb.f77_blas_function;

    hook_pos_zlaqgb ++;
    if( hook_pos_zlaqgb < __flexiblas_hooks->zlaqgb.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlaqgb.f77_hook_function[hook_pos_zlaqgb];
        fn_hook((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed, ( flexiblas_fortran_charlen_t ) len_equed);
    } else {
        hook_pos_zlaqgb = 0;
        fn((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed, ( flexiblas_fortran_charlen_t ) len_equed);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlaqgb(void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed, flexiblas_fortran_charlen_t len_equed) __attribute__((alias("flexiblas_chain_zlaqgb_")));
#else
void flexiblas_chain_zlaqgb(void* m, void* n, void* kl, void* ku, void* ab, void* ldab, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* equed, flexiblas_fortran_charlen_t len_equed){flexiblas_chain_zlaqgb_((void*) m, (void*) n, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) equed, (flexiblas_fortran_charlen_t) len_equed);}
#endif



