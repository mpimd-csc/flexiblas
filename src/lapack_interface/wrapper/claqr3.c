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


static TLS_STORE uint8_t hook_pos_claqr3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claqr3,CLAQR3)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, blasint* ns, blasint* nd, float complex* sh, float complex* v, blasint* ldv, blasint* nh, float complex* t, blasint* ldt, blasint* nv, float complex* wv, blasint* ldwv, float complex* work, blasint* lwork)
#else
void FC_GLOBAL(claqr3,CLAQR3)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, blasint* ns, blasint* nd, float complex* sh, float complex* v, blasint* ldv, blasint* nh, float complex* t, blasint* ldt, blasint* nv, float complex* wv, blasint* ldwv, float complex* work, blasint* lwork)
#endif
{
    void (*fn) (void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork);
    void (*fn_hook) (void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.claqr3.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->claqr3.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork);
        return;
    } else {
        hook_pos_claqr3 = 0;
        fn_hook((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(claqr3,CLAQR3)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, blasint* ns, blasint* nd, float complex* sh, float complex* v, blasint* ldv, blasint* nh, float complex* t, blasint* ldt, blasint* nv, float complex* wv, blasint* ldwv, float complex* work, blasint* lwork) __attribute__((alias(MTS(FC_GLOBAL(claqr3,CLAQR3)))));
void FC_GLOBAL3(claqr3,CLAQR3)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, blasint* ns, blasint* nd, float complex* sh, float complex* v, blasint* ldv, blasint* nh, float complex* t, blasint* ldt, blasint* nv, float complex* wv, blasint* ldwv, float complex* work, blasint* lwork) __attribute__((alias(MTS(FC_GLOBAL(claqr3,CLAQR3)))));
#else
void FC_GLOBAL2(claqr3,CLAQR3)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, blasint* ns, blasint* nd, float complex* sh, float complex* v, blasint* ldv, blasint* nh, float complex* t, blasint* ldt, blasint* nv, float complex* wv, blasint* ldwv, float complex* work, blasint* lwork){ FC_GLOBAL(claqr3,CLAQR3)((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork); }
void FC_GLOBAL3(claqr3,CLAQR3)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, float complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, float complex* z, blasint* ldz, blasint* ns, blasint* nd, float complex* sh, float complex* v, blasint* ldv, blasint* nh, float complex* t, blasint* ldt, blasint* nv, float complex* wv, blasint* ldwv, float complex* work, blasint* lwork){ FC_GLOBAL(claqr3,CLAQR3)((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claqr3_(void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork)
{
    void (*fn) (void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork);

    *(void **) & fn = current_backend->lapack.claqr3.f77_blas_function;

    fn((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork);

    return;
}
#ifndef __APPLE__
void flexiblas_real_claqr3(void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork) __attribute__((alias("flexiblas_real_claqr3_")));
#else
void flexiblas_real_claqr3(void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork){flexiblas_real_claqr3_((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_claqr3_(void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork)
{
    void (*fn) (void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork);
    void (*fn_hook) (void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork);

    *(void **) &fn      = current_backend->lapack.claqr3.f77_blas_function;

    hook_pos_claqr3 ++;
    if( hook_pos_claqr3 < __flexiblas_hooks->claqr3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->claqr3.f77_hook_function[hook_pos_claqr3];
        fn_hook((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork);
    } else {
        hook_pos_claqr3 = 0;
        fn((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_claqr3(void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork) __attribute__((alias("flexiblas_chain_claqr3_")));
#else
void flexiblas_chain_claqr3(void* wantt, void* wantz, void* n, void* ktop, void* kbot, void* nw, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* ns, void* nd, void* sh, void* v, void* ldv, void* nh, void* t, void* ldt, void* nv, void* wv, void* ldwv, void* work, void* lwork){flexiblas_chain_claqr3_((void*) wantt, (void*) wantz, (void*) n, (void*) ktop, (void*) kbot, (void*) nw, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) sh, (void*) v, (void*) ldv, (void*) nh, (void*) t, (void*) ldt, (void*) nv, (void*) wv, (void*) ldwv, (void*) work, (void*) lwork);}
#endif



