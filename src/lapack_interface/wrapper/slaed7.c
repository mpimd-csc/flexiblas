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


static TLS_STORE uint8_t hook_pos_slaed7 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaed7,SLAED7)(blasint* icompq, blasint* n, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(slaed7,SLAED7)(blasint* icompq, blasint* n, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float* work, blasint* iwork, blasint* info)
#endif
{
    void (*fn) (void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info);
    void (*fn_hook) (void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slaed7.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slaed7.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info);
        return;
    } else {
        hook_pos_slaed7 = 0;
        fn_hook((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slaed7,SLAED7)(blasint* icompq, blasint* n, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaed7,SLAED7)))));
void FC_GLOBAL3(slaed7,SLAED7)(blasint* icompq, blasint* n, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaed7,SLAED7)))));
#else
void FC_GLOBAL2(slaed7,SLAED7)(blasint* icompq, blasint* n, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float* work, blasint* iwork, blasint* info){ FC_GLOBAL(slaed7,SLAED7)((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info); }
void FC_GLOBAL3(slaed7,SLAED7)(blasint* icompq, blasint* n, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float* work, blasint* iwork, blasint* info){ FC_GLOBAL(slaed7,SLAED7)((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaed7_(void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info)
{
    void (*fn) (void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info);

    *(void **) & fn = current_backend->lapack.slaed7.f77_blas_function;

    fn((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slaed7(void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_real_slaed7_")));
#else
void flexiblas_real_slaed7(void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info){flexiblas_real_slaed7_((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slaed7_(void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info)
{
    void (*fn) (void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info);
    void (*fn_hook) (void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info);

    *(void **) &fn      = current_backend->lapack.slaed7.f77_blas_function;

    hook_pos_slaed7 ++;
    if( hook_pos_slaed7 < __flexiblas_hooks->slaed7.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slaed7.f77_hook_function[hook_pos_slaed7];
        fn_hook((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_slaed7 = 0;
        fn((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slaed7(void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_chain_slaed7_")));
#else
void flexiblas_chain_slaed7(void* icompq, void* n, void* qsiz, void* tlvls, void* curlvl, void* curpbm, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* qstore, void* qptr, void* prmptr, void* perm, void* givptr, void* givcol, void* givnum, void* work, void* iwork, void* info){flexiblas_chain_slaed7_((void*) icompq, (void*) n, (void*) qsiz, (void*) tlvls, (void*) curlvl, (void*) curpbm, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) qstore, (void*) qptr, (void*) prmptr, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) work, (void*) iwork, (void*) info);}
#endif



