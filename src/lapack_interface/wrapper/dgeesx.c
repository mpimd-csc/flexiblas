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


static TLS_STORE uint8_t hook_pos_dgeesx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dgeesx,DGEESX)(char* jobvs, char* sort, blasint* select, char* sense, blasint* n, double* a, blasint* lda, blasint* sdim, double* wr, double* wi, double* vs, blasint* ldvs, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense)
#else
void FC_GLOBAL(dgeesx,DGEESX)(char* jobvs, char* sort, blasint* select, char* sense, blasint* n, double* a, blasint* lda, blasint* sdim, double* wr, double* wi, double* vs, blasint* ldvs, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense)
#endif
{
    void (*fn) (void* jobvs, void* sort, void* select, void* sense, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);
    void (*fn_hook) (void* jobvs, void* sort, void* select, void* sense, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dgeesx.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dgeesx.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobvs, (void*) sort, (void*) select, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort, ( flexiblas_fortran_charlen_t ) len_sense);
        return;
    } else {
        hook_pos_dgeesx = 0;
        fn_hook((void*) jobvs, (void*) sort, (void*) select, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort, ( flexiblas_fortran_charlen_t ) len_sense);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dgeesx_(char* jobvs, char* sort, blasint* select, char* sense, blasint* n, double* a, blasint* lda, blasint* sdim, double* wr, double* wi, double* vs, blasint* ldvs, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense) __attribute__((alias(MTS(FC_GLOBAL(dgeesx,DGEESX)))));
#else
#ifndef __APPLE__
void dgeesx(char* jobvs, char* sort, blasint* select, char* sense, blasint* n, double* a, blasint* lda, blasint* sdim, double* wr, double* wi, double* vs, blasint* ldvs, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense) __attribute__((alias(MTS(FC_GLOBAL(dgeesx,DGEESX)))));
#else
void dgeesx(char* jobvs, char* sort, blasint* select, char* sense, blasint* n, double* a, blasint* lda, blasint* sdim, double* wr, double* wi, double* vs, blasint* ldvs, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense){ FC_GLOBAL(dgeesx,DGEESX)((void*) jobvs, (void*) sort, (void*) select, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvs, (flexiblas_fortran_charlen_t) len_sort, (flexiblas_fortran_charlen_t) len_sense); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dgeesx_(void* jobvs, void* sort, void* select, void* sense, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense)
{
    void (*fn) (void* jobvs, void* sort, void* select, void* sense, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    *(void **) & fn = current_backend->lapack.dgeesx.f77_blas_function;

    fn((void*) jobvs, (void*) sort, (void*) select, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort, ( flexiblas_fortran_charlen_t ) len_sense);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dgeesx(void* jobvs, void* sort, void* select, void* sense, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense) __attribute__((alias("flexiblas_real_dgeesx_")));
#else
void flexiblas_real_dgeesx(void* jobvs, void* sort, void* select, void* sense, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense){flexiblas_real_dgeesx_((void*) jobvs, (void*) sort, (void*) select, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvs, (flexiblas_fortran_charlen_t) len_sort, (flexiblas_fortran_charlen_t) len_sense);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dgeesx_(void* jobvs, void* sort, void* select, void* sense, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense)
{
    void (*fn) (void* jobvs, void* sort, void* select, void* sense, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);
    void (*fn_hook) (void* jobvs, void* sort, void* select, void* sense, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    *(void **) &fn      = current_backend->lapack.dgeesx.f77_blas_function;

    hook_pos_dgeesx ++;
    if( hook_pos_dgeesx < __flexiblas_hooks->dgeesx.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dgeesx.f77_hook_function[hook_pos_dgeesx];
        fn_hook((void*) jobvs, (void*) sort, (void*) select, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort, ( flexiblas_fortran_charlen_t ) len_sense);
    } else {
        hook_pos_dgeesx = 0;
        fn((void*) jobvs, (void*) sort, (void*) select, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort, ( flexiblas_fortran_charlen_t ) len_sense);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dgeesx(void* jobvs, void* sort, void* select, void* sense, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense) __attribute__((alias("flexiblas_chain_dgeesx_")));
#else
void flexiblas_chain_dgeesx(void* jobvs, void* sort, void* select, void* sense, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense){flexiblas_chain_dgeesx_((void*) jobvs, (void*) sort, (void*) select, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvs, (flexiblas_fortran_charlen_t) len_sort, (flexiblas_fortran_charlen_t) len_sense);}
#endif



