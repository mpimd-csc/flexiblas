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


static TLS_STORE uint8_t hook_pos_dstevd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dstevd,DSTEVD)(char* jobz, blasint* n, double* d, double* e, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz)
#else
void FC_GLOBAL(dstevd,DSTEVD)(char* jobz, blasint* n, double* d, double* e, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz)
#endif
{
    void (*fn) (void* jobz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobz);
    void (*fn_hook) (void* jobz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobz);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dstevd.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dstevd.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);
        return;
    } else {
        hook_pos_dstevd = 0;
        fn_hook((void*) jobz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dstevd,DSTEVD)(char* jobz, blasint* n, double* d, double* e, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz) __attribute__((alias(MTS(FC_GLOBAL(dstevd,DSTEVD)))));
void FC_GLOBAL3(dstevd,DSTEVD)(char* jobz, blasint* n, double* d, double* e, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz) __attribute__((alias(MTS(FC_GLOBAL(dstevd,DSTEVD)))));
#else
void FC_GLOBAL2(dstevd,DSTEVD)(char* jobz, blasint* n, double* d, double* e, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz){ FC_GLOBAL(dstevd,DSTEVD)((void*) jobz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobz); }
void FC_GLOBAL3(dstevd,DSTEVD)(char* jobz, blasint* n, double* d, double* e, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz){ FC_GLOBAL(dstevd,DSTEVD)((void*) jobz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobz); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dstevd_(void* jobz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobz)
{
    void (*fn) (void* jobz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobz);

    *(void **) & fn = current_backend->lapack.dstevd.f77_blas_function;

    fn((void*) jobz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dstevd(void* jobz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobz) __attribute__((alias("flexiblas_real_dstevd_")));
#else
void flexiblas_real_dstevd(void* jobz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobz){flexiblas_real_dstevd_((void*) jobz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobz);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dstevd_(void* jobz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobz)
{
    void (*fn) (void* jobz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobz);
    void (*fn_hook) (void* jobz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobz);

    *(void **) &fn      = current_backend->lapack.dstevd.f77_blas_function;

    hook_pos_dstevd ++;
    if( hook_pos_dstevd < __flexiblas_hooks->dstevd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dstevd.f77_hook_function[hook_pos_dstevd];
        fn_hook((void*) jobz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);
    } else {
        hook_pos_dstevd = 0;
        fn((void*) jobz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobz);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dstevd(void* jobz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobz) __attribute__((alias("flexiblas_chain_dstevd_")));
#else
void flexiblas_chain_dstevd(void* jobz, void* n, void* d, void* e, void* z, void* ldz, void* work, void* lwork, void* iwork, void* liwork, void* info, flexiblas_fortran_charlen_t len_jobz){flexiblas_chain_dstevd_((void*) jobz, (void*) n, (void*) d, (void*) e, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobz);}
#endif



