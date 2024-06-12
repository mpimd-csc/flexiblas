//  SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2024 Martin Koehler

   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with this program. If not, see <https://www.gnu.org/licenses/>.
   */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_ztrsen = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ztrsen,ZTRSEN)(char* job, char* compq, blasint* select, blasint* n, double complex* t, blasint* ldt, double complex* q, blasint* ldq, double complex* w, blasint* m, double* s, double* sep, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq)
#else
void FC_GLOBAL(ztrsen,ZTRSEN)(char* job, char* compq, blasint* select, blasint* n, double complex* t, blasint* ldt, double complex* q, blasint* ldq, double complex* w, blasint* m, double* s, double* sep, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq)
#endif
{
    void (*fn) (void* job, void* compq, void* select, void* n, void* t, void* ldt, void* q, void* ldq, void* w, void* m, void* s, void* sep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq);
    void (*fn_hook) (void* job, void* compq, void* select, void* n, void* t, void* ldt, void* q, void* ldq, void* w, void* m, void* s, void* sep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ztrsen.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ztrsen.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) job, (void*) compq, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) w, (void*) m, (void*) s, (void*) sep, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compq);
        return;
    } else {
        hook_pos_ztrsen = 0;
        fn_hook((void*) job, (void*) compq, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) w, (void*) m, (void*) s, (void*) sep, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compq);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void ztrsen_(char* job, char* compq, blasint* select, blasint* n, double complex* t, blasint* ldt, double complex* q, blasint* ldq, double complex* w, blasint* m, double* s, double* sep, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq) __attribute__((alias(MTS(FC_GLOBAL(ztrsen,ZTRSEN)))));
#else
#ifndef __APPLE__
void ztrsen(char* job, char* compq, blasint* select, blasint* n, double complex* t, blasint* ldt, double complex* q, blasint* ldq, double complex* w, blasint* m, double* s, double* sep, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq) __attribute__((alias(MTS(FC_GLOBAL(ztrsen,ZTRSEN)))));
#else
void ztrsen(char* job, char* compq, blasint* select, blasint* n, double complex* t, blasint* ldt, double complex* q, blasint* ldq, double complex* w, blasint* m, double* s, double* sep, double complex* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq){ FC_GLOBAL(ztrsen,ZTRSEN)((void*) job, (void*) compq, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) w, (void*) m, (void*) s, (void*) sep, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_compq); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ztrsen_(void* job, void* compq, void* select, void* n, void* t, void* ldt, void* q, void* ldq, void* w, void* m, void* s, void* sep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq)
{
    void (*fn) (void* job, void* compq, void* select, void* n, void* t, void* ldt, void* q, void* ldq, void* w, void* m, void* s, void* sep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq);

    *(void **) & fn = current_backend->lapack.ztrsen.f77_blas_function;

    fn((void*) job, (void*) compq, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) w, (void*) m, (void*) s, (void*) sep, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compq);

    return;
}
#ifndef __APPLE__
void flexiblas_real_ztrsen(void* job, void* compq, void* select, void* n, void* t, void* ldt, void* q, void* ldq, void* w, void* m, void* s, void* sep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq) __attribute__((alias("flexiblas_real_ztrsen_")));
#else
void flexiblas_real_ztrsen(void* job, void* compq, void* select, void* n, void* t, void* ldt, void* q, void* ldq, void* w, void* m, void* s, void* sep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq){flexiblas_real_ztrsen_((void*) job, (void*) compq, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) w, (void*) m, (void*) s, (void*) sep, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_compq);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ztrsen_(void* job, void* compq, void* select, void* n, void* t, void* ldt, void* q, void* ldq, void* w, void* m, void* s, void* sep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq)
{
    void (*fn) (void* job, void* compq, void* select, void* n, void* t, void* ldt, void* q, void* ldq, void* w, void* m, void* s, void* sep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq);
    void (*fn_hook) (void* job, void* compq, void* select, void* n, void* t, void* ldt, void* q, void* ldq, void* w, void* m, void* s, void* sep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq);

    *(void **) &fn      = current_backend->lapack.ztrsen.f77_blas_function;

    hook_pos_ztrsen ++;
    if( hook_pos_ztrsen < __flexiblas_hooks->ztrsen.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ztrsen.f77_hook_function[hook_pos_ztrsen];
        fn_hook((void*) job, (void*) compq, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) w, (void*) m, (void*) s, (void*) sep, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compq);
    } else {
        hook_pos_ztrsen = 0;
        fn((void*) job, (void*) compq, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) w, (void*) m, (void*) s, (void*) sep, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_compq);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_ztrsen(void* job, void* compq, void* select, void* n, void* t, void* ldt, void* q, void* ldq, void* w, void* m, void* s, void* sep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq) __attribute__((alias("flexiblas_chain_ztrsen_")));
#else
void flexiblas_chain_ztrsen(void* job, void* compq, void* select, void* n, void* t, void* ldt, void* q, void* ldq, void* w, void* m, void* s, void* sep, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq){flexiblas_chain_ztrsen_((void*) job, (void*) compq, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) q, (void*) ldq, (void*) w, (void*) m, (void*) s, (void*) sep, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_compq);}
#endif



