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


#ifndef FLEXIBLAS_CHARLEN_T
#define FLEXIBLAS_CHARLEN_T
#if __GNUC__ > 7
typedef size_t flexiblas_fortran_charlen_t;
#else
typedef int flexiblas_fortran_charlen_t;
#endif
#endif

#ifndef blasint
#ifdef FLEXIBLAS_INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif
#endif



static TLS_STORE uint8_t hook_pos_sorbdb = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sorbdb,SORBDB)(char* trans, char* signs, blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x12, blasint* ldx12, float* x21, blasint* ldx21, float* x22, blasint* ldx22, float* theta, float* phi, float* taup1, float* taup2, float* tauq1, float* tauq2, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs)
#else
void FC_GLOBAL(sorbdb,SORBDB)(char* trans, char* signs, blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x12, blasint* ldx12, float* x21, blasint* ldx21, float* x22, blasint* ldx22, float* theta, float* phi, float* taup1, float* taup2, float* tauq1, float* tauq2, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs)
#endif
{
	void (*fn) (void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* phi, void* taup1, void* taup2, void* tauq1, void* tauq2, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);
	void (*fn_hook) (void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* phi, void* taup1, void* taup2, void* tauq1, void* tauq2, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.sorbdb.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->sorbdb.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) phi, (void*) taup1, (void*) taup2, (void*) tauq1, (void*) tauq2, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_signs); 
		return;
	} else {
		hook_pos_sorbdb = 0;
		fn_hook((void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) phi, (void*) taup1, (void*) taup2, (void*) tauq1, (void*) tauq2, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_signs);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sorbdb_(char* trans, char* signs, blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x12, blasint* ldx12, float* x21, blasint* ldx21, float* x22, blasint* ldx22, float* theta, float* phi, float* taup1, float* taup2, float* tauq1, float* tauq2, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs) __attribute__((alias(MTS(FC_GLOBAL(sorbdb,SORBDB)))));
#else
#ifndef __APPLE__
void sorbdb(char* trans, char* signs, blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x12, blasint* ldx12, float* x21, blasint* ldx21, float* x22, blasint* ldx22, float* theta, float* phi, float* taup1, float* taup2, float* tauq1, float* tauq2, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs) __attribute__((alias(MTS(FC_GLOBAL(sorbdb,SORBDB)))));
#else
void sorbdb(char* trans, char* signs, blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x12, blasint* ldx12, float* x21, blasint* ldx21, float* x22, blasint* ldx22, float* theta, float* phi, float* taup1, float* taup2, float* tauq1, float* tauq2, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs){ FC_GLOBAL(sorbdb,SORBDB)((void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) phi, (void*) taup1, (void*) taup2, (void*) tauq1, (void*) tauq2, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_signs); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sorbdb_(void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* phi, void* taup1, void* taup2, void* tauq1, void* tauq2, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs)
{
	void (*fn) (void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* phi, void* taup1, void* taup2, void* tauq1, void* tauq2, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

	*(void **) & fn = current_backend->lapack.sorbdb.f77_blas_function; 

		fn((void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) phi, (void*) taup1, (void*) taup2, (void*) tauq1, (void*) tauq2, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_signs); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_sorbdb(void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* phi, void* taup1, void* taup2, void* tauq1, void* tauq2, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs) __attribute__((alias("flexiblas_real_sorbdb_")));
#else
void flexiblas_real_sorbdb(void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* phi, void* taup1, void* taup2, void* tauq1, void* tauq2, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs){flexiblas_real_sorbdb_((void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) phi, (void*) taup1, (void*) taup2, (void*) tauq1, (void*) tauq2, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_signs);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sorbdb_(void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* phi, void* taup1, void* taup2, void* tauq1, void* tauq2, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs)
{
	void (*fn) (void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* phi, void* taup1, void* taup2, void* tauq1, void* tauq2, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);
	void (*fn_hook) (void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* phi, void* taup1, void* taup2, void* tauq1, void* tauq2, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

	*(void **) &fn      = current_backend->lapack.sorbdb.f77_blas_function; 

    hook_pos_sorbdb ++;
    if( hook_pos_sorbdb < __flexiblas_hooks->sorbdb.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sorbdb.f77_hook_function[hook_pos_sorbdb];
        fn_hook((void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) phi, (void*) taup1, (void*) taup2, (void*) tauq1, (void*) tauq2, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_signs);
    } else {
        hook_pos_sorbdb = 0;
		fn((void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) phi, (void*) taup1, (void*) taup2, (void*) tauq1, (void*) tauq2, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_trans, ( flexiblas_fortran_charlen_t ) len_signs); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_sorbdb(void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* phi, void* taup1, void* taup2, void* tauq1, void* tauq2, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs) __attribute__((alias("flexiblas_chain_sorbdb_")));
#else
void flexiblas_chain_sorbdb(void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* phi, void* taup1, void* taup2, void* tauq1, void* tauq2, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs){flexiblas_chain_sorbdb_((void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) phi, (void*) taup1, (void*) taup2, (void*) tauq1, (void*) tauq2, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_trans, (flexiblas_fortran_charlen_t) len_signs);}
#endif



