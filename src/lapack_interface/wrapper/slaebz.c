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


static TLS_STORE uint8_t hook_pos_slaebz = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaebz,SLAEBZ)(blasint* ijob, blasint* nitmax, blasint* n, blasint* mmax, blasint* minp, blasint* nbmin, float* abstol, float* reltol, float* pivmin, float* d, float* e, float* e2, blasint* nval, float* ab, float* c, blasint* mout, blasint* nab, float* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(slaebz,SLAEBZ)(blasint* ijob, blasint* nitmax, blasint* n, blasint* mmax, blasint* minp, blasint* nbmin, float* abstol, float* reltol, float* pivmin, float* d, float* e, float* e2, blasint* nval, float* ab, float* c, blasint* mout, blasint* nab, float* work, blasint* iwork, blasint* info)
#endif
{
    void (*fn) (void* ijob, void* nitmax, void* n, void* mmax, void* minp, void* nbmin, void* abstol, void* reltol, void* pivmin, void* d, void* e, void* e2, void* nval, void* ab, void* c, void* mout, void* nab, void* work, void* iwork, void* info);
    void (*fn_hook) (void* ijob, void* nitmax, void* n, void* mmax, void* minp, void* nbmin, void* abstol, void* reltol, void* pivmin, void* d, void* e, void* e2, void* nval, void* ab, void* c, void* mout, void* nab, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slaebz.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slaebz.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) ijob, (void*) nitmax, (void*) n, (void*) mmax, (void*) minp, (void*) nbmin, (void*) abstol, (void*) reltol, (void*) pivmin, (void*) d, (void*) e, (void*) e2, (void*) nval, (void*) ab, (void*) c, (void*) mout, (void*) nab, (void*) work, (void*) iwork, (void*) info);
        return;
    } else {
        hook_pos_slaebz = 0;
        fn_hook((void*) ijob, (void*) nitmax, (void*) n, (void*) mmax, (void*) minp, (void*) nbmin, (void*) abstol, (void*) reltol, (void*) pivmin, (void*) d, (void*) e, (void*) e2, (void*) nval, (void*) ab, (void*) c, (void*) mout, (void*) nab, (void*) work, (void*) iwork, (void*) info);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void slaebz_(blasint* ijob, blasint* nitmax, blasint* n, blasint* mmax, blasint* minp, blasint* nbmin, float* abstol, float* reltol, float* pivmin, float* d, float* e, float* e2, blasint* nval, float* ab, float* c, blasint* mout, blasint* nab, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaebz,SLAEBZ)))));
#else
#ifndef __APPLE__
void slaebz(blasint* ijob, blasint* nitmax, blasint* n, blasint* mmax, blasint* minp, blasint* nbmin, float* abstol, float* reltol, float* pivmin, float* d, float* e, float* e2, blasint* nval, float* ab, float* c, blasint* mout, blasint* nab, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaebz,SLAEBZ)))));
#else
void slaebz(blasint* ijob, blasint* nitmax, blasint* n, blasint* mmax, blasint* minp, blasint* nbmin, float* abstol, float* reltol, float* pivmin, float* d, float* e, float* e2, blasint* nval, float* ab, float* c, blasint* mout, blasint* nab, float* work, blasint* iwork, blasint* info){ FC_GLOBAL(slaebz,SLAEBZ)((void*) ijob, (void*) nitmax, (void*) n, (void*) mmax, (void*) minp, (void*) nbmin, (void*) abstol, (void*) reltol, (void*) pivmin, (void*) d, (void*) e, (void*) e2, (void*) nval, (void*) ab, (void*) c, (void*) mout, (void*) nab, (void*) work, (void*) iwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaebz_(void* ijob, void* nitmax, void* n, void* mmax, void* minp, void* nbmin, void* abstol, void* reltol, void* pivmin, void* d, void* e, void* e2, void* nval, void* ab, void* c, void* mout, void* nab, void* work, void* iwork, void* info)
{
    void (*fn) (void* ijob, void* nitmax, void* n, void* mmax, void* minp, void* nbmin, void* abstol, void* reltol, void* pivmin, void* d, void* e, void* e2, void* nval, void* ab, void* c, void* mout, void* nab, void* work, void* iwork, void* info);

    *(void **) & fn = current_backend->lapack.slaebz.f77_blas_function;

    fn((void*) ijob, (void*) nitmax, (void*) n, (void*) mmax, (void*) minp, (void*) nbmin, (void*) abstol, (void*) reltol, (void*) pivmin, (void*) d, (void*) e, (void*) e2, (void*) nval, (void*) ab, (void*) c, (void*) mout, (void*) nab, (void*) work, (void*) iwork, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slaebz(void* ijob, void* nitmax, void* n, void* mmax, void* minp, void* nbmin, void* abstol, void* reltol, void* pivmin, void* d, void* e, void* e2, void* nval, void* ab, void* c, void* mout, void* nab, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_real_slaebz_")));
#else
void flexiblas_real_slaebz(void* ijob, void* nitmax, void* n, void* mmax, void* minp, void* nbmin, void* abstol, void* reltol, void* pivmin, void* d, void* e, void* e2, void* nval, void* ab, void* c, void* mout, void* nab, void* work, void* iwork, void* info){flexiblas_real_slaebz_((void*) ijob, (void*) nitmax, (void*) n, (void*) mmax, (void*) minp, (void*) nbmin, (void*) abstol, (void*) reltol, (void*) pivmin, (void*) d, (void*) e, (void*) e2, (void*) nval, (void*) ab, (void*) c, (void*) mout, (void*) nab, (void*) work, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slaebz_(void* ijob, void* nitmax, void* n, void* mmax, void* minp, void* nbmin, void* abstol, void* reltol, void* pivmin, void* d, void* e, void* e2, void* nval, void* ab, void* c, void* mout, void* nab, void* work, void* iwork, void* info)
{
    void (*fn) (void* ijob, void* nitmax, void* n, void* mmax, void* minp, void* nbmin, void* abstol, void* reltol, void* pivmin, void* d, void* e, void* e2, void* nval, void* ab, void* c, void* mout, void* nab, void* work, void* iwork, void* info);
    void (*fn_hook) (void* ijob, void* nitmax, void* n, void* mmax, void* minp, void* nbmin, void* abstol, void* reltol, void* pivmin, void* d, void* e, void* e2, void* nval, void* ab, void* c, void* mout, void* nab, void* work, void* iwork, void* info);

    *(void **) &fn      = current_backend->lapack.slaebz.f77_blas_function;

    hook_pos_slaebz ++;
    if( hook_pos_slaebz < __flexiblas_hooks->slaebz.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slaebz.f77_hook_function[hook_pos_slaebz];
        fn_hook((void*) ijob, (void*) nitmax, (void*) n, (void*) mmax, (void*) minp, (void*) nbmin, (void*) abstol, (void*) reltol, (void*) pivmin, (void*) d, (void*) e, (void*) e2, (void*) nval, (void*) ab, (void*) c, (void*) mout, (void*) nab, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_slaebz = 0;
        fn((void*) ijob, (void*) nitmax, (void*) n, (void*) mmax, (void*) minp, (void*) nbmin, (void*) abstol, (void*) reltol, (void*) pivmin, (void*) d, (void*) e, (void*) e2, (void*) nval, (void*) ab, (void*) c, (void*) mout, (void*) nab, (void*) work, (void*) iwork, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slaebz(void* ijob, void* nitmax, void* n, void* mmax, void* minp, void* nbmin, void* abstol, void* reltol, void* pivmin, void* d, void* e, void* e2, void* nval, void* ab, void* c, void* mout, void* nab, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_chain_slaebz_")));
#else
void flexiblas_chain_slaebz(void* ijob, void* nitmax, void* n, void* mmax, void* minp, void* nbmin, void* abstol, void* reltol, void* pivmin, void* d, void* e, void* e2, void* nval, void* ab, void* c, void* mout, void* nab, void* work, void* iwork, void* info){flexiblas_chain_slaebz_((void*) ijob, (void*) nitmax, (void*) n, (void*) mmax, (void*) minp, (void*) nbmin, (void*) abstol, (void*) reltol, (void*) pivmin, (void*) d, (void*) e, (void*) e2, (void*) nval, (void*) ab, (void*) c, (void*) mout, (void*) nab, (void*) work, (void*) iwork, (void*) info);}
#endif



