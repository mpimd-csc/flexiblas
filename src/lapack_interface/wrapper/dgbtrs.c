//    SPDX-License-Identifier: LGPL-3.0-or-later
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


static TLS_STORE uint8_t hook_pos_dgbtrs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dgbtrs,DGBTRS)(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, double* ab, blasint* ldab, blasint* ipiv, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans)
#else
void FC_GLOBAL(dgbtrs,DGBTRS)(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, double* ab, blasint* ldab, blasint* ipiv, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans)
#endif
{
	void (*fn) (void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_trans);
	void (*fn_hook) (void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_trans);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dgbtrs.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dgbtrs.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans); 
		return;
	} else {
		hook_pos_dgbtrs = 0;
		fn_hook((void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dgbtrs_(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, double* ab, blasint* ldab, blasint* ipiv, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(dgbtrs,DGBTRS)))));
#else
#ifndef __APPLE__
void dgbtrs(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, double* ab, blasint* ldab, blasint* ipiv, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(dgbtrs,DGBTRS)))));
#else
void dgbtrs(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, double* ab, blasint* ldab, blasint* ipiv, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(dgbtrs,DGBTRS)((void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info, (flexiblas_fortran_charlen_t) len_trans); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dgbtrs_(void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_trans)
{
	void (*fn) (void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_trans);

	*(void **) & fn = current_backend->lapack.dgbtrs.f77_blas_function; 

		fn((void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dgbtrs(void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_dgbtrs_")));
#else
void flexiblas_real_dgbtrs(void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_trans){flexiblas_real_dgbtrs_((void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dgbtrs_(void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_trans)
{
	void (*fn) (void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_trans);
	void (*fn_hook) (void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_trans);

	*(void **) &fn      = current_backend->lapack.dgbtrs.f77_blas_function; 

    hook_pos_dgbtrs ++;
    if( hook_pos_dgbtrs < __flexiblas_hooks->dgbtrs.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dgbtrs.f77_hook_function[hook_pos_dgbtrs];
        fn_hook((void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans);
    } else {
        hook_pos_dgbtrs = 0;
		fn((void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dgbtrs(void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_dgbtrs_")));
#else
void flexiblas_chain_dgbtrs(void* trans, void* n, void* kl, void* ku, void* nrhs, void* ab, void* ldab, void* ipiv, void* b, void* ldb, void* info, flexiblas_fortran_charlen_t len_trans){flexiblas_chain_dgbtrs_((void*) trans, (void*) n, (void*) kl, (void*) ku, (void*) nrhs, (void*) ab, (void*) ldab, (void*) ipiv, (void*) b, (void*) ldb, (void*) info, (flexiblas_fortran_charlen_t) len_trans);}
#endif



