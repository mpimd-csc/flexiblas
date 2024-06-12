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


static TLS_STORE uint8_t hook_pos_zlantp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(zlantp,ZLANTP)(char* norm, char* uplo, char* diag, blasint* n, double complex* ap, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
#else
double FC_GLOBAL(zlantp,ZLANTP)(char* norm, char* uplo, char* diag, blasint* n, double complex* ap, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
#endif
{
    double (*fn) (void* norm, void* uplo, void* diag, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
    double (*fn_hook) (void* norm, void* uplo, void* diag, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
    double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlantp.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlantp.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag);
        return ret;
    } else {
        hook_pos_zlantp = 0;
        ret = fn_hook((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag);
        return ret;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
double zlantp_(char* norm, char* uplo, char* diag, blasint* n, double complex* ap, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(zlantp,ZLANTP)))));
#else
#ifndef __APPLE__
double zlantp(char* norm, char* uplo, char* diag, blasint* n, double complex* ap, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(zlantp,ZLANTP)))));
#else
double zlantp(char* norm, char* uplo, char* diag, blasint* n, double complex* ap, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag){ return FC_GLOBAL(zlantp,ZLANTP)((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_diag); }
#endif
#endif




/* Real Implementation for Hooks */


double flexiblas_real_zlantp_(void* norm, void* uplo, void* diag, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
{
    double (*fn) (void* norm, void* uplo, void* diag, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
    double ret;

    *(void **) & fn = current_backend->lapack.zlantp.f77_blas_function;

    ret = fn((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag);

    return ret;
}
#ifndef __APPLE__
double flexiblas_real_zlantp(void* norm, void* uplo, void* diag, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_real_zlantp_")));
#else
double flexiblas_real_zlantp(void* norm, void* uplo, void* diag, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag){return flexiblas_real_zlantp_((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_diag);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_zlantp_(void* norm, void* uplo, void* diag, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
{
    double (*fn) (void* norm, void* uplo, void* diag, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
    double (*fn_hook) (void* norm, void* uplo, void* diag, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
    double ret;

    *(void **) &fn      = current_backend->lapack.zlantp.f77_blas_function;

    hook_pos_zlantp ++;
    if( hook_pos_zlantp < __flexiblas_hooks->zlantp.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlantp.f77_hook_function[hook_pos_zlantp];
        ret = fn_hook((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work, ( flexiblas_fortran_charlen_t )len_norm, ( flexiblas_fortran_charlen_t )len_uplo, ( flexiblas_fortran_charlen_t )len_diag);
    } else {
        hook_pos_zlantp = 0;
        ret = fn((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag);
    }
    return ret;
}
#ifndef __APPLE__
double flexiblas_chain_zlantp(void* norm, void* uplo, void* diag, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_chain_zlantp_")));
#else
double flexiblas_chain_zlantp(void* norm, void* uplo, void* diag, void* n, void* ap, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag){return flexiblas_chain_zlantp_((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_diag);}
#endif



