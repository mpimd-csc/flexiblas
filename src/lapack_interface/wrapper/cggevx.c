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


static TLS_STORE uint8_t hook_pos_cggevx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cggevx,CGGEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* alpha, float complex* beta, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* lscale, float* rscale, float* abnrm, float* bbnrm, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense)
#else
void FC_GLOBAL(cggevx,CGGEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* alpha, float complex* beta, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* lscale, float* rscale, float* abnrm, float* bbnrm, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense)
#endif
{
    void (*fn) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);
    void (*fn_hook) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.cggevx.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->cggevx.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_balanc, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr, ( flexiblas_fortran_charlen_t ) len_sense);
        return;
    } else {
        hook_pos_cggevx = 0;
        fn_hook((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_balanc, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr, ( flexiblas_fortran_charlen_t ) len_sense);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(cggevx,CGGEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* alpha, float complex* beta, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* lscale, float* rscale, float* abnrm, float* bbnrm, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense) __attribute__((alias(MTS(FC_GLOBAL(cggevx,CGGEVX)))));
void FC_GLOBAL3(cggevx,CGGEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* alpha, float complex* beta, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* lscale, float* rscale, float* abnrm, float* bbnrm, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense) __attribute__((alias(MTS(FC_GLOBAL(cggevx,CGGEVX)))));
#else
void FC_GLOBAL2(cggevx,CGGEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* alpha, float complex* beta, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* lscale, float* rscale, float* abnrm, float* bbnrm, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense){ FC_GLOBAL(cggevx,CGGEVX)((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_balanc, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr, (flexiblas_fortran_charlen_t) len_sense); }
void FC_GLOBAL3(cggevx,CGGEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* alpha, float complex* beta, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* lscale, float* rscale, float* abnrm, float* bbnrm, float* rconde, float* rcondv, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense){ FC_GLOBAL(cggevx,CGGEVX)((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_balanc, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr, (flexiblas_fortran_charlen_t) len_sense); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cggevx_(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense)
{
    void (*fn) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    *(void **) & fn = current_backend->lapack.cggevx.f77_blas_function;

    fn((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_balanc, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr, ( flexiblas_fortran_charlen_t ) len_sense);

    return;
}
#ifndef __APPLE__
void flexiblas_real_cggevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense) __attribute__((alias("flexiblas_real_cggevx_")));
#else
void flexiblas_real_cggevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense){flexiblas_real_cggevx_((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_balanc, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr, (flexiblas_fortran_charlen_t) len_sense);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cggevx_(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense)
{
    void (*fn) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);
    void (*fn_hook) (void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    *(void **) &fn      = current_backend->lapack.cggevx.f77_blas_function;

    hook_pos_cggevx ++;
    if( hook_pos_cggevx < __flexiblas_hooks->cggevx.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cggevx.f77_hook_function[hook_pos_cggevx];
        fn_hook((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_balanc, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr, ( flexiblas_fortran_charlen_t ) len_sense);
    } else {
        hook_pos_cggevx = 0;
        fn((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_balanc, ( flexiblas_fortran_charlen_t ) len_jobvl, ( flexiblas_fortran_charlen_t ) len_jobvr, ( flexiblas_fortran_charlen_t ) len_sense);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_cggevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense) __attribute__((alias("flexiblas_chain_cggevx_")));
#else
void flexiblas_chain_cggevx(void* balanc, void* jobvl, void* jobvr, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* vl, void* ldvl, void* vr, void* ldvr, void* ilo, void* ihi, void* lscale, void* rscale, void* abnrm, void* bbnrm, void* rconde, void* rcondv, void* work, void* lwork, void* rwork, void* iwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense){flexiblas_chain_cggevx_((void*) balanc, (void*) jobvl, (void*) jobvr, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) ilo, (void*) ihi, (void*) lscale, (void*) rscale, (void*) abnrm, (void*) bbnrm, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_balanc, (flexiblas_fortran_charlen_t) len_jobvl, (flexiblas_fortran_charlen_t) len_jobvr, (flexiblas_fortran_charlen_t) len_sense);}
#endif



