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


static TLS_STORE uint8_t hook_pos_slarrc = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slarrc,SLARRC)(char* jobt, blasint* n, float* vl, float* vu, float* d, float* e, float* pivmin, blasint* eigcnt, blasint* lcnt, blasint* rcnt, blasint* info, flexiblas_fortran_charlen_t len_jobt)
#else
void FC_GLOBAL(slarrc,SLARRC)(char* jobt, blasint* n, float* vl, float* vu, float* d, float* e, float* pivmin, blasint* eigcnt, blasint* lcnt, blasint* rcnt, blasint* info, flexiblas_fortran_charlen_t len_jobt)
#endif
{
    void (*fn) (void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt);
    void (*fn_hook) (void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slarrc.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slarrc.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobt);
        return;
    } else {
        hook_pos_slarrc = 0;
        fn_hook((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobt);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void slarrc_(char* jobt, blasint* n, float* vl, float* vu, float* d, float* e, float* pivmin, blasint* eigcnt, blasint* lcnt, blasint* rcnt, blasint* info, flexiblas_fortran_charlen_t len_jobt) __attribute__((alias(MTS(FC_GLOBAL(slarrc,SLARRC)))));
#else
#ifndef __APPLE__
void slarrc(char* jobt, blasint* n, float* vl, float* vu, float* d, float* e, float* pivmin, blasint* eigcnt, blasint* lcnt, blasint* rcnt, blasint* info, flexiblas_fortran_charlen_t len_jobt) __attribute__((alias(MTS(FC_GLOBAL(slarrc,SLARRC)))));
#else
void slarrc(char* jobt, blasint* n, float* vl, float* vu, float* d, float* e, float* pivmin, blasint* eigcnt, blasint* lcnt, blasint* rcnt, blasint* info, flexiblas_fortran_charlen_t len_jobt){ FC_GLOBAL(slarrc,SLARRC)((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, (flexiblas_fortran_charlen_t) len_jobt); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slarrc_(void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt)
{
    void (*fn) (void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt);

    *(void **) & fn = current_backend->lapack.slarrc.f77_blas_function;

    fn((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobt);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slarrc(void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt) __attribute__((alias("flexiblas_real_slarrc_")));
#else
void flexiblas_real_slarrc(void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt){flexiblas_real_slarrc_((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, (flexiblas_fortran_charlen_t) len_jobt);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slarrc_(void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt)
{
    void (*fn) (void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt);
    void (*fn_hook) (void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt);

    *(void **) &fn      = current_backend->lapack.slarrc.f77_blas_function;

    hook_pos_slarrc ++;
    if( hook_pos_slarrc < __flexiblas_hooks->slarrc.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slarrc.f77_hook_function[hook_pos_slarrc];
        fn_hook((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobt);
    } else {
        hook_pos_slarrc = 0;
        fn((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobt);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slarrc(void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt) __attribute__((alias("flexiblas_chain_slarrc_")));
#else
void flexiblas_chain_slarrc(void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt){flexiblas_chain_slarrc_((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, (flexiblas_fortran_charlen_t) len_jobt);}
#endif



