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


static TLS_STORE uint8_t hook_pos_zlaed8 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlaed8,ZLAED8)(blasint* k, blasint* n, blasint* qsiz, double complex* q, blasint* ldq, double* d, double* rho, blasint* cutpnt, double* z, double* dlambda, double complex* q2, blasint* ldq2, double* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* info)
#else
void FC_GLOBAL(zlaed8,ZLAED8)(blasint* k, blasint* n, blasint* qsiz, double complex* q, blasint* ldq, double* d, double* rho, blasint* cutpnt, double* z, double* dlambda, double complex* q2, blasint* ldq2, double* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* info)
#endif
{
    void (*fn) (void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info);
    void (*fn_hook) (void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlaed8.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlaed8.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info);
        return;
    } else {
        hook_pos_zlaed8 = 0;
        fn_hook((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(zlaed8,ZLAED8)(blasint* k, blasint* n, blasint* qsiz, double complex* q, blasint* ldq, double* d, double* rho, blasint* cutpnt, double* z, double* dlambda, double complex* q2, blasint* ldq2, double* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zlaed8,ZLAED8)))));
void FC_GLOBAL3(zlaed8,ZLAED8)(blasint* k, blasint* n, blasint* qsiz, double complex* q, blasint* ldq, double* d, double* rho, blasint* cutpnt, double* z, double* dlambda, double complex* q2, blasint* ldq2, double* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zlaed8,ZLAED8)))));
#else
void FC_GLOBAL2(zlaed8,ZLAED8)(blasint* k, blasint* n, blasint* qsiz, double complex* q, blasint* ldq, double* d, double* rho, blasint* cutpnt, double* z, double* dlambda, double complex* q2, blasint* ldq2, double* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* info){ FC_GLOBAL(zlaed8,ZLAED8)((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info); }
void FC_GLOBAL3(zlaed8,ZLAED8)(blasint* k, blasint* n, blasint* qsiz, double complex* q, blasint* ldq, double* d, double* rho, blasint* cutpnt, double* z, double* dlambda, double complex* q2, blasint* ldq2, double* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* info){ FC_GLOBAL(zlaed8,ZLAED8)((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaed8_(void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info)
{
    void (*fn) (void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info);

    *(void **) & fn = current_backend->lapack.zlaed8.f77_blas_function;

    fn((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlaed8(void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info) __attribute__((alias("flexiblas_real_zlaed8_")));
#else
void flexiblas_real_zlaed8(void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info){flexiblas_real_zlaed8_((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlaed8_(void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info)
{
    void (*fn) (void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info);
    void (*fn_hook) (void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info);

    *(void **) &fn      = current_backend->lapack.zlaed8.f77_blas_function;

    hook_pos_zlaed8 ++;
    if( hook_pos_zlaed8 < __flexiblas_hooks->zlaed8.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlaed8.f77_hook_function[hook_pos_zlaed8];
        fn_hook((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info);
    } else {
        hook_pos_zlaed8 = 0;
        fn((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlaed8(void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info) __attribute__((alias("flexiblas_chain_zlaed8_")));
#else
void flexiblas_chain_zlaed8(void* k, void* n, void* qsiz, void* q, void* ldq, void* d, void* rho, void* cutpnt, void* z, void* dlambda, void* q2, void* ldq2, void* w, void* indxp, void* indx, void* indxq, void* perm, void* givptr, void* givcol, void* givnum, void* info){flexiblas_chain_zlaed8_((void*) k, (void*) n, (void*) qsiz, (void*) q, (void*) ldq, (void*) d, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlambda, (void*) q2, (void*) ldq2, (void*) w, (void*) indxp, (void*) indx, (void*) indxq, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) info);}
#endif



