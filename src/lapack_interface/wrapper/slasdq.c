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


static TLS_STORE uint8_t hook_pos_slasdq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasdq,SLASDQ)(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, float* d, float* e, float* vt, blasint* ldvt, float* u, blasint* ldu, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(slasdq,SLASDQ)(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, float* d, float* e, float* vt, blasint* ldvt, float* u, blasint* ldu, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    void (*fn) (void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slasdq.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slasdq.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    } else {
        hook_pos_slasdq = 0;
        fn_hook((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slasdq,SLASDQ)(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, float* d, float* e, float* vt, blasint* ldvt, float* u, blasint* ldu, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(slasdq,SLASDQ)))));
void FC_GLOBAL3(slasdq,SLASDQ)(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, float* d, float* e, float* vt, blasint* ldvt, float* u, blasint* ldu, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(slasdq,SLASDQ)))));
#else
void FC_GLOBAL2(slasdq,SLASDQ)(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, float* d, float* e, float* vt, blasint* ldvt, float* u, blasint* ldu, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(slasdq,SLASDQ)((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
void FC_GLOBAL3(slasdq,SLASDQ)(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, float* d, float* e, float* vt, blasint* ldvt, float* u, blasint* ldu, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(slasdq,SLASDQ)((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_uplo); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasdq_(void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) & fn = current_backend->lapack.slasdq.f77_blas_function;

    fn((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slasdq(void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_slasdq_")));
#else
void flexiblas_real_slasdq(void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_slasdq_((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slasdq_(void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_uplo);

    *(void **) &fn      = current_backend->lapack.slasdq.f77_blas_function;

    hook_pos_slasdq ++;
    if( hook_pos_slasdq < __flexiblas_hooks->slasdq.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slasdq.f77_hook_function[hook_pos_slasdq];
        fn_hook((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_slasdq = 0;
        fn((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slasdq(void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_slasdq_")));
#else
void flexiblas_chain_slasdq(void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_slasdq_((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



