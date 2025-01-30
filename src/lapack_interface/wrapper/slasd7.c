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


static TLS_STORE uint8_t hook_pos_slasd7 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasd7,SLASD7)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* zw, float* vf, float* vfw, float* vl, float* vlw, float* alpha, float* beta, float* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* c, float* s, blasint* info)
#else
void FC_GLOBAL(slasd7,SLASD7)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* zw, float* vf, float* vfw, float* vl, float* vlw, float* alpha, float* beta, float* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* c, float* s, blasint* info)
#endif
{
    void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);
    void (*fn_hook) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slasd7.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slasd7.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info);
        return;
    } else {
        hook_pos_slasd7 = 0;
        fn_hook((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slasd7,SLASD7)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* zw, float* vf, float* vfw, float* vl, float* vlw, float* alpha, float* beta, float* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* c, float* s, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slasd7,SLASD7)))));
void FC_GLOBAL3(slasd7,SLASD7)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* zw, float* vf, float* vfw, float* vl, float* vlw, float* alpha, float* beta, float* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* c, float* s, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slasd7,SLASD7)))));
#else
void FC_GLOBAL2(slasd7,SLASD7)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* zw, float* vf, float* vfw, float* vl, float* vlw, float* alpha, float* beta, float* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* c, float* s, blasint* info){ FC_GLOBAL(slasd7,SLASD7)((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info); }
void FC_GLOBAL3(slasd7,SLASD7)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* zw, float* vf, float* vfw, float* vl, float* vlw, float* alpha, float* beta, float* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* c, float* s, blasint* info){ FC_GLOBAL(slasd7,SLASD7)((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasd7_(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info)
{
    void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);

    *(void **) & fn = current_backend->lapack.slasd7.f77_blas_function;

    fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slasd7(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info) __attribute__((alias("flexiblas_real_slasd7_")));
#else
void flexiblas_real_slasd7(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info){flexiblas_real_slasd7_((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slasd7_(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info)
{
    void (*fn) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);
    void (*fn_hook) (void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info);

    *(void **) &fn      = current_backend->lapack.slasd7.f77_blas_function;

    hook_pos_slasd7 ++;
    if( hook_pos_slasd7 < __flexiblas_hooks->slasd7.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slasd7.f77_hook_function[hook_pos_slasd7];
        fn_hook((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info);
    } else {
        hook_pos_slasd7 = 0;
        fn((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slasd7(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info) __attribute__((alias("flexiblas_chain_slasd7_")));
#else
void flexiblas_chain_slasd7(void* icompq, void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* zw, void* vf, void* vfw, void* vl, void* vlw, void* alpha, void* beta, void* dsigma, void* idx, void* idxp, void* idxq, void* perm, void* givptr, void* givcol, void* ldgcol, void* givnum, void* ldgnum, void* c, void* s, void* info){flexiblas_chain_slasd7_((void*) icompq, (void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) zw, (void*) vf, (void*) vfw, (void*) vl, (void*) vlw, (void*) alpha, (void*) beta, (void*) dsigma, (void*) idx, (void*) idxp, (void*) idxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) ldgcol, (void*) givnum, (void*) ldgnum, (void*) c, (void*) s, (void*) info);}
#endif



