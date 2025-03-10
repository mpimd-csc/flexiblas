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


static TLS_STORE uint8_t hook_pos_sbdsdc = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sbdsdc,SBDSDC)(char* uplo, char* compq, blasint* n, float* d, float* e, float* u, blasint* ldu, float* vt, blasint* ldvt, float* q, blasint* iq, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq)
#else
void FC_GLOBAL(sbdsdc,SBDSDC)(char* uplo, char* compq, blasint* n, float* d, float* e, float* u, blasint* ldu, float* vt, blasint* ldvt, float* q, blasint* iq, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq)
#endif
{
    void (*fn) (void* uplo, void* compq, void* n, void* d, void* e, void* u, void* ldu, void* vt, void* ldvt, void* q, void* iq, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq);
    void (*fn_hook) (void* uplo, void* compq, void* n, void* d, void* e, void* u, void* ldu, void* vt, void* ldvt, void* q, void* iq, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.sbdsdc.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->sbdsdc.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) uplo, (void*) compq, (void*) n, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) q, (void*) iq, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_compq);
        return;
    } else {
        hook_pos_sbdsdc = 0;
        fn_hook((void*) uplo, (void*) compq, (void*) n, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) q, (void*) iq, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_compq);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(sbdsdc,SBDSDC)(char* uplo, char* compq, blasint* n, float* d, float* e, float* u, blasint* ldu, float* vt, blasint* ldvt, float* q, blasint* iq, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq) __attribute__((alias(MTS(FC_GLOBAL(sbdsdc,SBDSDC)))));
void FC_GLOBAL3(sbdsdc,SBDSDC)(char* uplo, char* compq, blasint* n, float* d, float* e, float* u, blasint* ldu, float* vt, blasint* ldvt, float* q, blasint* iq, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq) __attribute__((alias(MTS(FC_GLOBAL(sbdsdc,SBDSDC)))));
#else
void FC_GLOBAL2(sbdsdc,SBDSDC)(char* uplo, char* compq, blasint* n, float* d, float* e, float* u, blasint* ldu, float* vt, blasint* ldvt, float* q, blasint* iq, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq){ FC_GLOBAL(sbdsdc,SBDSDC)((void*) uplo, (void*) compq, (void*) n, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) q, (void*) iq, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_compq); }
void FC_GLOBAL3(sbdsdc,SBDSDC)(char* uplo, char* compq, blasint* n, float* d, float* e, float* u, blasint* ldu, float* vt, blasint* ldvt, float* q, blasint* iq, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq){ FC_GLOBAL(sbdsdc,SBDSDC)((void*) uplo, (void*) compq, (void*) n, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) q, (void*) iq, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_compq); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sbdsdc_(void* uplo, void* compq, void* n, void* d, void* e, void* u, void* ldu, void* vt, void* ldvt, void* q, void* iq, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq)
{
    void (*fn) (void* uplo, void* compq, void* n, void* d, void* e, void* u, void* ldu, void* vt, void* ldvt, void* q, void* iq, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq);

    *(void **) & fn = current_backend->lapack.sbdsdc.f77_blas_function;

    fn((void*) uplo, (void*) compq, (void*) n, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) q, (void*) iq, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_compq);

    return;
}
#ifndef __APPLE__
void flexiblas_real_sbdsdc(void* uplo, void* compq, void* n, void* d, void* e, void* u, void* ldu, void* vt, void* ldvt, void* q, void* iq, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq) __attribute__((alias("flexiblas_real_sbdsdc_")));
#else
void flexiblas_real_sbdsdc(void* uplo, void* compq, void* n, void* d, void* e, void* u, void* ldu, void* vt, void* ldvt, void* q, void* iq, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq){flexiblas_real_sbdsdc_((void*) uplo, (void*) compq, (void*) n, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) q, (void*) iq, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_compq);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sbdsdc_(void* uplo, void* compq, void* n, void* d, void* e, void* u, void* ldu, void* vt, void* ldvt, void* q, void* iq, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq)
{
    void (*fn) (void* uplo, void* compq, void* n, void* d, void* e, void* u, void* ldu, void* vt, void* ldvt, void* q, void* iq, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq);
    void (*fn_hook) (void* uplo, void* compq, void* n, void* d, void* e, void* u, void* ldu, void* vt, void* ldvt, void* q, void* iq, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq);

    *(void **) &fn      = current_backend->lapack.sbdsdc.f77_blas_function;

    hook_pos_sbdsdc ++;
    if( hook_pos_sbdsdc < __flexiblas_hooks->sbdsdc.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sbdsdc.f77_hook_function[hook_pos_sbdsdc];
        fn_hook((void*) uplo, (void*) compq, (void*) n, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) q, (void*) iq, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_compq);
    } else {
        hook_pos_sbdsdc = 0;
        fn((void*) uplo, (void*) compq, (void*) n, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) q, (void*) iq, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_compq);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_sbdsdc(void* uplo, void* compq, void* n, void* d, void* e, void* u, void* ldu, void* vt, void* ldvt, void* q, void* iq, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq) __attribute__((alias("flexiblas_chain_sbdsdc_")));
#else
void flexiblas_chain_sbdsdc(void* uplo, void* compq, void* n, void* d, void* e, void* u, void* ldu, void* vt, void* ldvt, void* q, void* iq, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq){flexiblas_chain_sbdsdc_((void*) uplo, (void*) compq, (void*) n, (void*) d, (void*) e, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) q, (void*) iq, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_compq);}
#endif



