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


static TLS_STORE uint8_t hook_pos_zgebak = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgebak,ZGEBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, double complex* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side)
#else
void FC_GLOBAL(zgebak,ZGEBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, double complex* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side)
#endif
{
    void (*fn) (void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);
    void (*fn_hook) (void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zgebak.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zgebak.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_side);
        return;
    } else {
        hook_pos_zgebak = 0;
        fn_hook((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_side);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zgebak,ZGEBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, double complex* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side) __attribute__((alias(MTS(FC_GLOBAL(zgebak,ZGEBAK)))));
void FC_GLOBAL3(zgebak,ZGEBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, double complex* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side) __attribute__((alias(MTS(FC_GLOBAL(zgebak,ZGEBAK)))));
#else
void FC_GLOBAL2(zgebak,ZGEBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, double complex* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side){ FC_GLOBAL(zgebak,ZGEBAK)((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_side); }
void FC_GLOBAL3(zgebak,ZGEBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, double complex* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side){ FC_GLOBAL(zgebak,ZGEBAK)((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_side); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgebak_(void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side)
{
    void (*fn) (void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);

    *(void **) & fn = current_backend->lapack.zgebak.f77_blas_function;

    fn((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_side);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zgebak(void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side) __attribute__((alias("flexiblas_real_zgebak_")));
#else
void flexiblas_real_zgebak(void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side){flexiblas_real_zgebak_((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_side);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgebak_(void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side)
{
    void (*fn) (void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);
    void (*fn_hook) (void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);

    *(void **) &fn      = current_backend->lapack.zgebak.f77_blas_function;

    hook_pos_zgebak ++;
    if( hook_pos_zgebak < __flexiblas_hooks->zgebak.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgebak.f77_hook_function[hook_pos_zgebak];
        fn_hook((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_side);
    } else {
        hook_pos_zgebak = 0;
        fn((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_side);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zgebak(void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side) __attribute__((alias("flexiblas_chain_zgebak_")));
#else
void flexiblas_chain_zgebak(void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side){flexiblas_chain_zgebak_((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_side);}
#endif



