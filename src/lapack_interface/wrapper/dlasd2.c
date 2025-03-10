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


static TLS_STORE uint8_t hook_pos_dlasd2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlasd2,DLASD2)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* z, double* alpha, double* beta, double* u, blasint* ldu, double* vt, blasint* ldvt, double* dsigma, double* u2, blasint* ldu2, double* vt2, blasint* ldvt2, blasint* idxp, blasint* idx, blasint* idxc, blasint* idxq, blasint* coltyp, blasint* info)
#else
void FC_GLOBAL(dlasd2,DLASD2)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* z, double* alpha, double* beta, double* u, blasint* ldu, double* vt, blasint* ldvt, double* dsigma, double* u2, blasint* ldu2, double* vt2, blasint* ldvt2, blasint* idxp, blasint* idx, blasint* idxc, blasint* idxq, blasint* coltyp, blasint* info)
#endif
{
    void (*fn) (void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* alpha, void* beta, void* u, void* ldu, void* vt, void* ldvt, void* dsigma, void* u2, void* ldu2, void* vt2, void* ldvt2, void* idxp, void* idx, void* idxc, void* idxq, void* coltyp, void* info);
    void (*fn_hook) (void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* alpha, void* beta, void* u, void* ldu, void* vt, void* ldvt, void* dsigma, void* u2, void* ldu2, void* vt2, void* ldvt2, void* idxp, void* idx, void* idxc, void* idxq, void* coltyp, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlasd2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlasd2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) dsigma, (void*) u2, (void*) ldu2, (void*) vt2, (void*) ldvt2, (void*) idxp, (void*) idx, (void*) idxc, (void*) idxq, (void*) coltyp, (void*) info);
        return;
    } else {
        hook_pos_dlasd2 = 0;
        fn_hook((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) dsigma, (void*) u2, (void*) ldu2, (void*) vt2, (void*) ldvt2, (void*) idxp, (void*) idx, (void*) idxc, (void*) idxq, (void*) coltyp, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlasd2,DLASD2)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* z, double* alpha, double* beta, double* u, blasint* ldu, double* vt, blasint* ldvt, double* dsigma, double* u2, blasint* ldu2, double* vt2, blasint* ldvt2, blasint* idxp, blasint* idx, blasint* idxc, blasint* idxq, blasint* coltyp, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasd2,DLASD2)))));
void FC_GLOBAL3(dlasd2,DLASD2)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* z, double* alpha, double* beta, double* u, blasint* ldu, double* vt, blasint* ldvt, double* dsigma, double* u2, blasint* ldu2, double* vt2, blasint* ldvt2, blasint* idxp, blasint* idx, blasint* idxc, blasint* idxq, blasint* coltyp, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasd2,DLASD2)))));
#else
void FC_GLOBAL2(dlasd2,DLASD2)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* z, double* alpha, double* beta, double* u, blasint* ldu, double* vt, blasint* ldvt, double* dsigma, double* u2, blasint* ldu2, double* vt2, blasint* ldvt2, blasint* idxp, blasint* idx, blasint* idxc, blasint* idxq, blasint* coltyp, blasint* info){ FC_GLOBAL(dlasd2,DLASD2)((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) dsigma, (void*) u2, (void*) ldu2, (void*) vt2, (void*) ldvt2, (void*) idxp, (void*) idx, (void*) idxc, (void*) idxq, (void*) coltyp, (void*) info); }
void FC_GLOBAL3(dlasd2,DLASD2)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* z, double* alpha, double* beta, double* u, blasint* ldu, double* vt, blasint* ldvt, double* dsigma, double* u2, blasint* ldu2, double* vt2, blasint* ldvt2, blasint* idxp, blasint* idx, blasint* idxc, blasint* idxq, blasint* coltyp, blasint* info){ FC_GLOBAL(dlasd2,DLASD2)((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) dsigma, (void*) u2, (void*) ldu2, (void*) vt2, (void*) ldvt2, (void*) idxp, (void*) idx, (void*) idxc, (void*) idxq, (void*) coltyp, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlasd2_(void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* alpha, void* beta, void* u, void* ldu, void* vt, void* ldvt, void* dsigma, void* u2, void* ldu2, void* vt2, void* ldvt2, void* idxp, void* idx, void* idxc, void* idxq, void* coltyp, void* info)
{
    void (*fn) (void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* alpha, void* beta, void* u, void* ldu, void* vt, void* ldvt, void* dsigma, void* u2, void* ldu2, void* vt2, void* ldvt2, void* idxp, void* idx, void* idxc, void* idxq, void* coltyp, void* info);

    *(void **) & fn = current_backend->lapack.dlasd2.f77_blas_function;

    fn((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) dsigma, (void*) u2, (void*) ldu2, (void*) vt2, (void*) ldvt2, (void*) idxp, (void*) idx, (void*) idxc, (void*) idxq, (void*) coltyp, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlasd2(void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* alpha, void* beta, void* u, void* ldu, void* vt, void* ldvt, void* dsigma, void* u2, void* ldu2, void* vt2, void* ldvt2, void* idxp, void* idx, void* idxc, void* idxq, void* coltyp, void* info) __attribute__((alias("flexiblas_real_dlasd2_")));
#else
void flexiblas_real_dlasd2(void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* alpha, void* beta, void* u, void* ldu, void* vt, void* ldvt, void* dsigma, void* u2, void* ldu2, void* vt2, void* ldvt2, void* idxp, void* idx, void* idxc, void* idxq, void* coltyp, void* info){flexiblas_real_dlasd2_((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) dsigma, (void*) u2, (void*) ldu2, (void*) vt2, (void*) ldvt2, (void*) idxp, (void*) idx, (void*) idxc, (void*) idxq, (void*) coltyp, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlasd2_(void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* alpha, void* beta, void* u, void* ldu, void* vt, void* ldvt, void* dsigma, void* u2, void* ldu2, void* vt2, void* ldvt2, void* idxp, void* idx, void* idxc, void* idxq, void* coltyp, void* info)
{
    void (*fn) (void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* alpha, void* beta, void* u, void* ldu, void* vt, void* ldvt, void* dsigma, void* u2, void* ldu2, void* vt2, void* ldvt2, void* idxp, void* idx, void* idxc, void* idxq, void* coltyp, void* info);
    void (*fn_hook) (void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* alpha, void* beta, void* u, void* ldu, void* vt, void* ldvt, void* dsigma, void* u2, void* ldu2, void* vt2, void* ldvt2, void* idxp, void* idx, void* idxc, void* idxq, void* coltyp, void* info);

    *(void **) &fn      = current_backend->lapack.dlasd2.f77_blas_function;

    hook_pos_dlasd2 ++;
    if( hook_pos_dlasd2 < __flexiblas_hooks->dlasd2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlasd2.f77_hook_function[hook_pos_dlasd2];
        fn_hook((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) dsigma, (void*) u2, (void*) ldu2, (void*) vt2, (void*) ldvt2, (void*) idxp, (void*) idx, (void*) idxc, (void*) idxq, (void*) coltyp, (void*) info);
    } else {
        hook_pos_dlasd2 = 0;
        fn((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) dsigma, (void*) u2, (void*) ldu2, (void*) vt2, (void*) ldvt2, (void*) idxp, (void*) idx, (void*) idxc, (void*) idxq, (void*) coltyp, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlasd2(void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* alpha, void* beta, void* u, void* ldu, void* vt, void* ldvt, void* dsigma, void* u2, void* ldu2, void* vt2, void* ldvt2, void* idxp, void* idx, void* idxc, void* idxq, void* coltyp, void* info) __attribute__((alias("flexiblas_chain_dlasd2_")));
#else
void flexiblas_chain_dlasd2(void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* alpha, void* beta, void* u, void* ldu, void* vt, void* ldvt, void* dsigma, void* u2, void* ldu2, void* vt2, void* ldvt2, void* idxp, void* idx, void* idxc, void* idxq, void* coltyp, void* info){flexiblas_chain_dlasd2_((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) dsigma, (void*) u2, (void*) ldu2, (void*) vt2, (void*) ldvt2, (void*) idxp, (void*) idx, (void*) idxc, (void*) idxq, (void*) coltyp, (void*) info);}
#endif



