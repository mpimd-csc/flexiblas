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


static TLS_STORE uint8_t hook_pos_clar1v = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clar1v,CLAR1V)(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float complex* z, blaslogical* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work)
#else
void FC_GLOBAL(clar1v,CLAR1V)(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float complex* z, blaslogical* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work)
#endif
{
    void (*fn) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);
    void (*fn_hook) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.clar1v.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->clar1v.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);
        return;
    } else {
        hook_pos_clar1v = 0;
        fn_hook((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(clar1v,CLAR1V)(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float complex* z, blaslogical* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work) __attribute__((alias(MTS(FC_GLOBAL(clar1v,CLAR1V)))));
void FC_GLOBAL3(clar1v,CLAR1V)(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float complex* z, blaslogical* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work) __attribute__((alias(MTS(FC_GLOBAL(clar1v,CLAR1V)))));
#else
void FC_GLOBAL2(clar1v,CLAR1V)(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float complex* z, blaslogical* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work){ FC_GLOBAL(clar1v,CLAR1V)((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work); }
void FC_GLOBAL3(clar1v,CLAR1V)(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float complex* z, blaslogical* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work){ FC_GLOBAL(clar1v,CLAR1V)((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clar1v_(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work)
{
    void (*fn) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);

    *(void **) & fn = current_backend->lapack.clar1v.f77_blas_function;

    fn((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);

    return;
}
#ifndef __APPLE__
void flexiblas_real_clar1v(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work) __attribute__((alias("flexiblas_real_clar1v_")));
#else
void flexiblas_real_clar1v(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work){flexiblas_real_clar1v_((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clar1v_(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work)
{
    void (*fn) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);
    void (*fn_hook) (void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work);

    *(void **) &fn      = current_backend->lapack.clar1v.f77_blas_function;

    hook_pos_clar1v ++;
    if( hook_pos_clar1v < __flexiblas_hooks->clar1v.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clar1v.f77_hook_function[hook_pos_clar1v];
        fn_hook((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);
    } else {
        hook_pos_clar1v = 0;
        fn((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_clar1v(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work) __attribute__((alias("flexiblas_chain_clar1v_")));
#else
void flexiblas_chain_clar1v(void* n, void* b1, void* bn, void* lambda, void* d, void* l, void* ld, void* lld, void* pivmin, void* gaptol, void* z, void* wantnc, void* negcnt, void* ztz, void* mingma, void* r, void* isuppz, void* nrminv, void* resid, void* rqcorr, void* work){flexiblas_chain_clar1v_((void*) n, (void*) b1, (void*) bn, (void*) lambda, (void*) d, (void*) l, (void*) ld, (void*) lld, (void*) pivmin, (void*) gaptol, (void*) z, (void*) wantnc, (void*) negcnt, (void*) ztz, (void*) mingma, (void*) r, (void*) isuppz, (void*) nrminv, (void*) resid, (void*) rqcorr, (void*) work);}
#endif



