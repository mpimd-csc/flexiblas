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


static TLS_STORE uint8_t hook_pos_zlaqr5 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlaqr5,ZLAQR5)(blaslogical* wantt, blaslogical* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, double complex* s, double complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, double complex* z, blasint* ldz, double complex* v, blasint* ldv, double complex* u, blasint* ldu, blasint* nv, double complex* wv, blasint* ldwv, blasint* nh, double complex* wh, blasint* ldwh)
#else
void FC_GLOBAL(zlaqr5,ZLAQR5)(blaslogical* wantt, blaslogical* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, double complex* s, double complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, double complex* z, blasint* ldz, double complex* v, blasint* ldv, double complex* u, blasint* ldu, blasint* nv, double complex* wv, blasint* ldwv, blasint* nh, double complex* wh, blasint* ldwh)
#endif
{
    void (*fn) (void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh);
    void (*fn_hook) (void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlaqr5.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlaqr5.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh);
        return;
    } else {
        hook_pos_zlaqr5 = 0;
        fn_hook((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zlaqr5,ZLAQR5)(blaslogical* wantt, blaslogical* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, double complex* s, double complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, double complex* z, blasint* ldz, double complex* v, blasint* ldv, double complex* u, blasint* ldu, blasint* nv, double complex* wv, blasint* ldwv, blasint* nh, double complex* wh, blasint* ldwh) __attribute__((alias(MTS(FC_GLOBAL(zlaqr5,ZLAQR5)))));
void FC_GLOBAL3(zlaqr5,ZLAQR5)(blaslogical* wantt, blaslogical* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, double complex* s, double complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, double complex* z, blasint* ldz, double complex* v, blasint* ldv, double complex* u, blasint* ldu, blasint* nv, double complex* wv, blasint* ldwv, blasint* nh, double complex* wh, blasint* ldwh) __attribute__((alias(MTS(FC_GLOBAL(zlaqr5,ZLAQR5)))));
#else
void FC_GLOBAL2(zlaqr5,ZLAQR5)(blaslogical* wantt, blaslogical* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, double complex* s, double complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, double complex* z, blasint* ldz, double complex* v, blasint* ldv, double complex* u, blasint* ldu, blasint* nv, double complex* wv, blasint* ldwv, blasint* nh, double complex* wh, blasint* ldwh){ FC_GLOBAL(zlaqr5,ZLAQR5)((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh); }
void FC_GLOBAL3(zlaqr5,ZLAQR5)(blaslogical* wantt, blaslogical* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, double complex* s, double complex* h, blasint* ldh, blasint* iloz, blasint* ihiz, double complex* z, blasint* ldz, double complex* v, blasint* ldv, double complex* u, blasint* ldu, blasint* nv, double complex* wv, blasint* ldwv, blasint* nh, double complex* wh, blasint* ldwh){ FC_GLOBAL(zlaqr5,ZLAQR5)((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaqr5_(void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh)
{
    void (*fn) (void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh);

    *(void **) & fn = current_backend->lapack.zlaqr5.f77_blas_function;

    fn((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlaqr5(void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh) __attribute__((alias("flexiblas_real_zlaqr5_")));
#else
void flexiblas_real_zlaqr5(void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh){flexiblas_real_zlaqr5_((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlaqr5_(void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh)
{
    void (*fn) (void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh);
    void (*fn_hook) (void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh);

    *(void **) &fn      = current_backend->lapack.zlaqr5.f77_blas_function;

    hook_pos_zlaqr5 ++;
    if( hook_pos_zlaqr5 < __flexiblas_hooks->zlaqr5.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlaqr5.f77_hook_function[hook_pos_zlaqr5];
        fn_hook((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh);
    } else {
        hook_pos_zlaqr5 = 0;
        fn((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlaqr5(void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh) __attribute__((alias("flexiblas_chain_zlaqr5_")));
#else
void flexiblas_chain_zlaqr5(void* wantt, void* wantz, void* kacc22, void* n, void* ktop, void* kbot, void* nshfts, void* s, void* h, void* ldh, void* iloz, void* ihiz, void* z, void* ldz, void* v, void* ldv, void* u, void* ldu, void* nv, void* wv, void* ldwv, void* nh, void* wh, void* ldwh){flexiblas_chain_zlaqr5_((void*) wantt, (void*) wantz, (void*) kacc22, (void*) n, (void*) ktop, (void*) kbot, (void*) nshfts, (void*) s, (void*) h, (void*) ldh, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) v, (void*) ldv, (void*) u, (void*) ldu, (void*) nv, (void*) wv, (void*) ldwv, (void*) nh, (void*) wh, (void*) ldwh);}
#endif



