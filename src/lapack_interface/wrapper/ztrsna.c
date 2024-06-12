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


static TLS_STORE uint8_t hook_pos_ztrsna = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ztrsna,ZTRSNA)(char* job, char* howmny, blasint* select, blasint* n, double complex* t, blasint* ldt, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double* s, double* sep, blasint* mm, blasint* m, double complex* work, blasint* ldwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny)
#else
void FC_GLOBAL(ztrsna,ZTRSNA)(char* job, char* howmny, blasint* select, blasint* n, double complex* t, blasint* ldt, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double* s, double* sep, blasint* mm, blasint* m, double complex* work, blasint* ldwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny)
#endif
{
    void (*fn) (void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);
    void (*fn_hook) (void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ztrsna.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ztrsna.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny);
        return;
    } else {
        hook_pos_ztrsna = 0;
        fn_hook((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void ztrsna_(char* job, char* howmny, blasint* select, blasint* n, double complex* t, blasint* ldt, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double* s, double* sep, blasint* mm, blasint* m, double complex* work, blasint* ldwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias(MTS(FC_GLOBAL(ztrsna,ZTRSNA)))));
#else
#ifndef __APPLE__
void ztrsna(char* job, char* howmny, blasint* select, blasint* n, double complex* t, blasint* ldt, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double* s, double* sep, blasint* mm, blasint* m, double complex* work, blasint* ldwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias(MTS(FC_GLOBAL(ztrsna,ZTRSNA)))));
#else
void ztrsna(char* job, char* howmny, blasint* select, blasint* n, double complex* t, blasint* ldt, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double* s, double* sep, blasint* mm, blasint* m, double complex* work, blasint* ldwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny){ FC_GLOBAL(ztrsna,ZTRSNA)((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_howmny); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ztrsna_(void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny)
{
    void (*fn) (void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    *(void **) & fn = current_backend->lapack.ztrsna.f77_blas_function;

    fn((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny);

    return;
}
#ifndef __APPLE__
void flexiblas_real_ztrsna(void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias("flexiblas_real_ztrsna_")));
#else
void flexiblas_real_ztrsna(void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny){flexiblas_real_ztrsna_((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_howmny);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ztrsna_(void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny)
{
    void (*fn) (void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);
    void (*fn_hook) (void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    *(void **) &fn      = current_backend->lapack.ztrsna.f77_blas_function;

    hook_pos_ztrsna ++;
    if( hook_pos_ztrsna < __flexiblas_hooks->ztrsna.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ztrsna.f77_hook_function[hook_pos_ztrsna];
        fn_hook((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny);
    } else {
        hook_pos_ztrsna = 0;
        fn((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_ztrsna(void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias("flexiblas_chain_ztrsna_")));
#else
void flexiblas_chain_ztrsna(void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny){flexiblas_chain_ztrsna_((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_howmny);}
#endif



