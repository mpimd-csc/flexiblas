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


static TLS_STORE uint8_t hook_pos_zlange = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(zlange,ZLANGE)(char* norm, blasint* m, blasint* n, double complex* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm)
#else
double FC_GLOBAL(zlange,ZLANGE)(char* norm, blasint* m, blasint* n, double complex* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm)
#endif
{
    double (*fn) (void* norm, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm);
    double (*fn_hook) (void* norm, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm);
    double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlange.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlange.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm);
        return ret;
    } else {
        hook_pos_zlange = 0;
        ret = fn_hook((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm);
        return ret;
    }
}
#ifndef __APPLE__
double FC_GLOBAL2(zlange,ZLANGE)(char* norm, blasint* m, blasint* n, double complex* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(zlange,ZLANGE)))));
double FC_GLOBAL3(zlange,ZLANGE)(char* norm, blasint* m, blasint* n, double complex* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(zlange,ZLANGE)))));
#else
double FC_GLOBAL2(zlange,ZLANGE)(char* norm, blasint* m, blasint* n, double complex* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm){ return FC_GLOBAL(zlange,ZLANGE)((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, (flexiblas_fortran_charlen_t) len_norm); }
double FC_GLOBAL3(zlange,ZLANGE)(char* norm, blasint* m, blasint* n, double complex* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm){ return FC_GLOBAL(zlange,ZLANGE)((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, (flexiblas_fortran_charlen_t) len_norm); }
#endif




/* Real Implementation for Hooks */


double flexiblas_real_zlange_(void* norm, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm)
{
    double (*fn) (void* norm, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm);
    double ret;

    *(void **) & fn = current_backend->lapack.zlange.f77_blas_function;

    ret = fn((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm);

    return ret;
}
#ifndef __APPLE__
double flexiblas_real_zlange(void* norm, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_real_zlange_")));
#else
double flexiblas_real_zlange(void* norm, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm){return flexiblas_real_zlange_((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, (flexiblas_fortran_charlen_t) len_norm);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_zlange_(void* norm, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm)
{
    double (*fn) (void* norm, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm);
    double (*fn_hook) (void* norm, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm);
    double ret;

    *(void **) &fn      = current_backend->lapack.zlange.f77_blas_function;

    hook_pos_zlange ++;
    if( hook_pos_zlange < __flexiblas_hooks->zlange.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlange.f77_hook_function[hook_pos_zlange];
        ret = fn_hook((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t )len_norm);
    } else {
        hook_pos_zlange = 0;
        ret = fn((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm);
    }
    return ret;
}
#ifndef __APPLE__
double flexiblas_chain_zlange(void* norm, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_chain_zlange_")));
#else
double flexiblas_chain_zlange(void* norm, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm){return flexiblas_chain_zlange_((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, (flexiblas_fortran_charlen_t) len_norm);}
#endif



