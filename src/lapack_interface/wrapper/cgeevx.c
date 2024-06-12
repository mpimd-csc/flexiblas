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


static TLS_STORE uint8_t hook_pos_cgeevx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgeevx,CGEEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float complex* a, blasint* lda, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* scale, float* abnrm, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense)
#else
void FC_GLOBAL(cgeevx,CGEEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float complex* a, blasint* lda, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* scale, float* abnrm, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense)
#endif
{
    void (*fn) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);
    void (*fn_hook) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.cgeevx.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->cgeevx.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_balanc, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr, ( flexiblas_fortran_charlen_t ) len_sense);
        return;
    } else {
        hook_pos_cgeevx = 0;
        fn_hook((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_balanc, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr, ( flexiblas_fortran_charlen_t ) len_sense);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void cgeevx_(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float complex* a, blasint* lda, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* scale, float* abnrm, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense) __attribute__((alias(MTS(FC_GLOBAL(cgeevx,CGEEVX)))));
#else
#ifndef __APPLE__
void cgeevx(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float complex* a, blasint* lda, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* scale, float* abnrm, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense) __attribute__((alias(MTS(FC_GLOBAL(cgeevx,CGEEVX)))));
#else
void cgeevx(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float complex* a, blasint* lda, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* scale, float* abnrm, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense){ FC_GLOBAL(cgeevx,CGEEVX)((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_balanc, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr, (flexiblas_fortran_charlen_t) len_sense); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgeevx_(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense)
{
    void (*fn) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    *(void **) & fn = current_backend->lapack.cgeevx.f77_blas_function;

    fn((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_balanc, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr, ( flexiblas_fortran_charlen_t ) len_sense);

    return;
}
#ifndef __APPLE__
void flexiblas_real_cgeevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense) __attribute__((alias("flexiblas_real_cgeevx_")));
#else
void flexiblas_real_cgeevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense){flexiblas_real_cgeevx_((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_balanc, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr, (flexiblas_fortran_charlen_t) len_sense);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgeevx_(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense)
{
    void (*fn) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);
    void (*fn_hook) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    *(void **) &fn      = current_backend->lapack.cgeevx.f77_blas_function;

    hook_pos_cgeevx ++;
    if( hook_pos_cgeevx < __flexiblas_hooks->cgeevx.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgeevx.f77_hook_function[hook_pos_cgeevx];
        fn_hook((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_balanc, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr, ( flexiblas_fortran_charlen_t ) len_sense);
    } else {
        hook_pos_cgeevx = 0;
        fn((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_balanc, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr, ( flexiblas_fortran_charlen_t ) len_sense);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_cgeevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense) __attribute__((alias("flexiblas_chain_cgeevx_")));
#else
void flexiblas_chain_cgeevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* scale, void* abnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense){flexiblas_chain_cgeevx_((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) scale, (void*) abnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_balanc, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr, (flexiblas_fortran_charlen_t) len_sense);}
#endif



