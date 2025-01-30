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


static TLS_STORE uint8_t hook_pos_sggesx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sggesx,SGGESX)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, char* sense, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* sdim, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense)
#else
void FC_GLOBAL(sggesx,SGGESX)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, char* sense, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* sdim, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense)
#endif
{
    void (*fn) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);
    void (*fn_hook) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.sggesx.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->sggesx.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr, ( flexiblas_fortran_charlen_t ) len_sort, ( flexiblas_fortran_charlen_t ) len_sense);
        return;
    } else {
        hook_pos_sggesx = 0;
        fn_hook((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr, ( flexiblas_fortran_charlen_t ) len_sort, ( flexiblas_fortran_charlen_t ) len_sense);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(sggesx,SGGESX)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, char* sense, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* sdim, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense) __attribute__((alias(MTS(FC_GLOBAL(sggesx,SGGESX)))));
void FC_GLOBAL3(sggesx,SGGESX)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, char* sense, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* sdim, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense) __attribute__((alias(MTS(FC_GLOBAL(sggesx,SGGESX)))));
#else
void FC_GLOBAL2(sggesx,SGGESX)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, char* sense, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* sdim, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense){ FC_GLOBAL(sggesx,SGGESX)((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr, (flexiblas_fortran_charlen_t) len_sort, (flexiblas_fortran_charlen_t) len_sense); }
void FC_GLOBAL3(sggesx,SGGESX)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, char* sense, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* sdim, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense){ FC_GLOBAL(sggesx,SGGESX)((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr, (flexiblas_fortran_charlen_t) len_sort, (flexiblas_fortran_charlen_t) len_sense); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sggesx_(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense)
{
    void (*fn) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    *(void **) & fn = current_backend->lapack.sggesx.f77_blas_function;

    fn((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr, ( flexiblas_fortran_charlen_t ) len_sort, ( flexiblas_fortran_charlen_t ) len_sense);

    return;
}
#ifndef __APPLE__
void flexiblas_real_sggesx(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense) __attribute__((alias("flexiblas_real_sggesx_")));
#else
void flexiblas_real_sggesx(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense){flexiblas_real_sggesx_((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr, (flexiblas_fortran_charlen_t) len_sort, (flexiblas_fortran_charlen_t) len_sense);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sggesx_(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense)
{
    void (*fn) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);
    void (*fn_hook) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    *(void **) &fn      = current_backend->lapack.sggesx.f77_blas_function;

    hook_pos_sggesx ++;
    if( hook_pos_sggesx < __flexiblas_hooks->sggesx.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sggesx.f77_hook_function[hook_pos_sggesx];
        fn_hook((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr, ( flexiblas_fortran_charlen_t ) len_sort, ( flexiblas_fortran_charlen_t ) len_sense);
    } else {
        hook_pos_sggesx = 0;
        fn((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvsl, ( flexiblas_fortran_charlen_t ) len_jobvsr, ( flexiblas_fortran_charlen_t ) len_sort, ( flexiblas_fortran_charlen_t ) len_sense);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_sggesx(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense) __attribute__((alias("flexiblas_chain_sggesx_")));
#else
void flexiblas_chain_sggesx(void* jobvsl, void* jobvsr, void* sort, void* selctg, void* sense, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* rconde, void* rcondv, void* work, void* lwork, void* iwork, void* liwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense){flexiblas_chain_sggesx_((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) sense, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) rconde, (void*) rcondv, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvsl, (flexiblas_fortran_charlen_t) len_jobvsr, (flexiblas_fortran_charlen_t) len_sort, (flexiblas_fortran_charlen_t) len_sense);}
#endif



