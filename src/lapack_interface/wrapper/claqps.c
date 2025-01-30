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


static TLS_STORE uint8_t hook_pos_claqps = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claqps,CLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float* vn1, float* vn2, float complex* auxv, float complex* f, blasint* ldf)
#else
void FC_GLOBAL(claqps,CLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float* vn1, float* vn2, float complex* auxv, float complex* f, blasint* ldf)
#endif
{
    void (*fn) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);
    void (*fn_hook) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.claqps.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->claqps.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);
        return;
    } else {
        hook_pos_claqps = 0;
        fn_hook((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(claqps,CLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float* vn1, float* vn2, float complex* auxv, float complex* f, blasint* ldf) __attribute__((alias(MTS(FC_GLOBAL(claqps,CLAQPS)))));
void FC_GLOBAL3(claqps,CLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float* vn1, float* vn2, float complex* auxv, float complex* f, blasint* ldf) __attribute__((alias(MTS(FC_GLOBAL(claqps,CLAQPS)))));
#else
void FC_GLOBAL2(claqps,CLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float* vn1, float* vn2, float complex* auxv, float complex* f, blasint* ldf){ FC_GLOBAL(claqps,CLAQPS)((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf); }
void FC_GLOBAL3(claqps,CLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float* vn1, float* vn2, float complex* auxv, float complex* f, blasint* ldf){ FC_GLOBAL(claqps,CLAQPS)((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claqps_(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf)
{
    void (*fn) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);

    *(void **) & fn = current_backend->lapack.claqps.f77_blas_function;

    fn((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);

    return;
}
#ifndef __APPLE__
void flexiblas_real_claqps(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf) __attribute__((alias("flexiblas_real_claqps_")));
#else
void flexiblas_real_claqps(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf){flexiblas_real_claqps_((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_claqps_(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf)
{
    void (*fn) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);
    void (*fn_hook) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);

    *(void **) &fn      = current_backend->lapack.claqps.f77_blas_function;

    hook_pos_claqps ++;
    if( hook_pos_claqps < __flexiblas_hooks->claqps.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->claqps.f77_hook_function[hook_pos_claqps];
        fn_hook((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);
    } else {
        hook_pos_claqps = 0;
        fn((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_claqps(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf) __attribute__((alias("flexiblas_chain_claqps_")));
#else
void flexiblas_chain_claqps(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf){flexiblas_chain_claqps_((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);}
#endif



