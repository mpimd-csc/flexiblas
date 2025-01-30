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


static TLS_STORE uint8_t hook_pos_zlaqp3rk = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlaqp3rk,ZLAQP3RK)(blasint* m, blasint* n, blasint* nrhs, blasint* ioffset, blasint* nb, double* abstol, double* reltol, blasint* kp1, double* maxc2nrm, double complex* a, blasint* lda, blaslogical* done, blasint* kb, double* maxc2nrmk, double* relmaxc2nrmk, blasint* jpiv, double complex* tau, double* vn1, double* vn2, double complex* auxv, double complex* f, blasint* ldf, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(zlaqp3rk,ZLAQP3RK)(blasint* m, blasint* n, blasint* nrhs, blasint* ioffset, blasint* nb, double* abstol, double* reltol, blasint* kp1, double* maxc2nrm, double complex* a, blasint* lda, blaslogical* done, blasint* kb, double* maxc2nrmk, double* relmaxc2nrmk, blasint* jpiv, double complex* tau, double* vn1, double* vn2, double complex* auxv, double complex* f, blasint* ldf, blasint* iwork, blasint* info)
#endif
{
    void (*fn) (void* m, void* n, void* nrhs, void* ioffset, void* nb, void* abstol, void* reltol, void* kp1, void* maxc2nrm, void* a, void* lda, void* done, void* kb, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf, void* iwork, void* info);
    void (*fn_hook) (void* m, void* n, void* nrhs, void* ioffset, void* nb, void* abstol, void* reltol, void* kp1, void* maxc2nrm, void* a, void* lda, void* done, void* kb, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlaqp3rk.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlaqp3rk.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) m, (void*) n, (void*) nrhs, (void*) ioffset, (void*) nb, (void*) abstol, (void*) reltol, (void*) kp1, (void*) maxc2nrm, (void*) a, (void*) lda, (void*) done, (void*) kb, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf, (void*) iwork, (void*) info);
        return;
    } else {
        hook_pos_zlaqp3rk = 0;
        fn_hook((void*) m, (void*) n, (void*) nrhs, (void*) ioffset, (void*) nb, (void*) abstol, (void*) reltol, (void*) kp1, (void*) maxc2nrm, (void*) a, (void*) lda, (void*) done, (void*) kb, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf, (void*) iwork, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zlaqp3rk,ZLAQP3RK)(blasint* m, blasint* n, blasint* nrhs, blasint* ioffset, blasint* nb, double* abstol, double* reltol, blasint* kp1, double* maxc2nrm, double complex* a, blasint* lda, blaslogical* done, blasint* kb, double* maxc2nrmk, double* relmaxc2nrmk, blasint* jpiv, double complex* tau, double* vn1, double* vn2, double complex* auxv, double complex* f, blasint* ldf, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zlaqp3rk,ZLAQP3RK)))));
void FC_GLOBAL3(zlaqp3rk,ZLAQP3RK)(blasint* m, blasint* n, blasint* nrhs, blasint* ioffset, blasint* nb, double* abstol, double* reltol, blasint* kp1, double* maxc2nrm, double complex* a, blasint* lda, blaslogical* done, blasint* kb, double* maxc2nrmk, double* relmaxc2nrmk, blasint* jpiv, double complex* tau, double* vn1, double* vn2, double complex* auxv, double complex* f, blasint* ldf, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zlaqp3rk,ZLAQP3RK)))));
#else
void FC_GLOBAL2(zlaqp3rk,ZLAQP3RK)(blasint* m, blasint* n, blasint* nrhs, blasint* ioffset, blasint* nb, double* abstol, double* reltol, blasint* kp1, double* maxc2nrm, double complex* a, blasint* lda, blaslogical* done, blasint* kb, double* maxc2nrmk, double* relmaxc2nrmk, blasint* jpiv, double complex* tau, double* vn1, double* vn2, double complex* auxv, double complex* f, blasint* ldf, blasint* iwork, blasint* info){ FC_GLOBAL(zlaqp3rk,ZLAQP3RK)((void*) m, (void*) n, (void*) nrhs, (void*) ioffset, (void*) nb, (void*) abstol, (void*) reltol, (void*) kp1, (void*) maxc2nrm, (void*) a, (void*) lda, (void*) done, (void*) kb, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf, (void*) iwork, (void*) info); }
void FC_GLOBAL3(zlaqp3rk,ZLAQP3RK)(blasint* m, blasint* n, blasint* nrhs, blasint* ioffset, blasint* nb, double* abstol, double* reltol, blasint* kp1, double* maxc2nrm, double complex* a, blasint* lda, blaslogical* done, blasint* kb, double* maxc2nrmk, double* relmaxc2nrmk, blasint* jpiv, double complex* tau, double* vn1, double* vn2, double complex* auxv, double complex* f, blasint* ldf, blasint* iwork, blasint* info){ FC_GLOBAL(zlaqp3rk,ZLAQP3RK)((void*) m, (void*) n, (void*) nrhs, (void*) ioffset, (void*) nb, (void*) abstol, (void*) reltol, (void*) kp1, (void*) maxc2nrm, (void*) a, (void*) lda, (void*) done, (void*) kb, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf, (void*) iwork, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaqp3rk_(void* m, void* n, void* nrhs, void* ioffset, void* nb, void* abstol, void* reltol, void* kp1, void* maxc2nrm, void* a, void* lda, void* done, void* kb, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf, void* iwork, void* info)
{
    void (*fn) (void* m, void* n, void* nrhs, void* ioffset, void* nb, void* abstol, void* reltol, void* kp1, void* maxc2nrm, void* a, void* lda, void* done, void* kb, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf, void* iwork, void* info);

    *(void **) & fn = current_backend->lapack.zlaqp3rk.f77_blas_function;

    fn((void*) m, (void*) n, (void*) nrhs, (void*) ioffset, (void*) nb, (void*) abstol, (void*) reltol, (void*) kp1, (void*) maxc2nrm, (void*) a, (void*) lda, (void*) done, (void*) kb, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf, (void*) iwork, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlaqp3rk(void* m, void* n, void* nrhs, void* ioffset, void* nb, void* abstol, void* reltol, void* kp1, void* maxc2nrm, void* a, void* lda, void* done, void* kb, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf, void* iwork, void* info) __attribute__((alias("flexiblas_real_zlaqp3rk_")));
#else
void flexiblas_real_zlaqp3rk(void* m, void* n, void* nrhs, void* ioffset, void* nb, void* abstol, void* reltol, void* kp1, void* maxc2nrm, void* a, void* lda, void* done, void* kb, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf, void* iwork, void* info){flexiblas_real_zlaqp3rk_((void*) m, (void*) n, (void*) nrhs, (void*) ioffset, (void*) nb, (void*) abstol, (void*) reltol, (void*) kp1, (void*) maxc2nrm, (void*) a, (void*) lda, (void*) done, (void*) kb, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlaqp3rk_(void* m, void* n, void* nrhs, void* ioffset, void* nb, void* abstol, void* reltol, void* kp1, void* maxc2nrm, void* a, void* lda, void* done, void* kb, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf, void* iwork, void* info)
{
    void (*fn) (void* m, void* n, void* nrhs, void* ioffset, void* nb, void* abstol, void* reltol, void* kp1, void* maxc2nrm, void* a, void* lda, void* done, void* kb, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf, void* iwork, void* info);
    void (*fn_hook) (void* m, void* n, void* nrhs, void* ioffset, void* nb, void* abstol, void* reltol, void* kp1, void* maxc2nrm, void* a, void* lda, void* done, void* kb, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf, void* iwork, void* info);

    *(void **) &fn      = current_backend->lapack.zlaqp3rk.f77_blas_function;

    hook_pos_zlaqp3rk ++;
    if( hook_pos_zlaqp3rk < __flexiblas_hooks->zlaqp3rk.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlaqp3rk.f77_hook_function[hook_pos_zlaqp3rk];
        fn_hook((void*) m, (void*) n, (void*) nrhs, (void*) ioffset, (void*) nb, (void*) abstol, (void*) reltol, (void*) kp1, (void*) maxc2nrm, (void*) a, (void*) lda, (void*) done, (void*) kb, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf, (void*) iwork, (void*) info);
    } else {
        hook_pos_zlaqp3rk = 0;
        fn((void*) m, (void*) n, (void*) nrhs, (void*) ioffset, (void*) nb, (void*) abstol, (void*) reltol, (void*) kp1, (void*) maxc2nrm, (void*) a, (void*) lda, (void*) done, (void*) kb, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf, (void*) iwork, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlaqp3rk(void* m, void* n, void* nrhs, void* ioffset, void* nb, void* abstol, void* reltol, void* kp1, void* maxc2nrm, void* a, void* lda, void* done, void* kb, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf, void* iwork, void* info) __attribute__((alias("flexiblas_chain_zlaqp3rk_")));
#else
void flexiblas_chain_zlaqp3rk(void* m, void* n, void* nrhs, void* ioffset, void* nb, void* abstol, void* reltol, void* kp1, void* maxc2nrm, void* a, void* lda, void* done, void* kb, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf, void* iwork, void* info){flexiblas_chain_zlaqp3rk_((void*) m, (void*) n, (void*) nrhs, (void*) ioffset, (void*) nb, (void*) abstol, (void*) reltol, (void*) kp1, (void*) maxc2nrm, (void*) a, (void*) lda, (void*) done, (void*) kb, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf, (void*) iwork, (void*) info);}
#endif



