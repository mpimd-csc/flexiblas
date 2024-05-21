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


static TLS_STORE uint8_t hook_pos_sorm22 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sorm22,SORM22)(char* side, char* trans, blasint* m, blasint* n, blasint* n1, blasint* n2, float* q, blasint* ldq, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
#else
void FC_GLOBAL(sorm22,SORM22)(char* side, char* trans, blasint* m, blasint* n, blasint* n1, blasint* n2, float* q, blasint* ldq, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
#endif
{
	void (*fn) (void* side, void* trans, void* m, void* n, void* n1, void* n2, void* q, void* ldq, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);
	void (*fn_hook) (void* side, void* trans, void* m, void* n, void* n1, void* n2, void* q, void* ldq, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.sorm22.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->sorm22.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) trans, (void*) m, (void*) n, (void*) n1, (void*) n2, (void*) q, (void*) ldq, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans); 
		return;
	} else {
		hook_pos_sorm22 = 0;
		fn_hook((void*) side, (void*) trans, (void*) m, (void*) n, (void*) n1, (void*) n2, (void*) q, (void*) ldq, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sorm22_(char* side, char* trans, blasint* m, blasint* n, blasint* n1, blasint* n2, float* q, blasint* ldq, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(sorm22,SORM22)))));
#else
#ifndef __APPLE__
void sorm22(char* side, char* trans, blasint* m, blasint* n, blasint* n1, blasint* n2, float* q, blasint* ldq, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(sorm22,SORM22)))));
#else
void sorm22(char* side, char* trans, blasint* m, blasint* n, blasint* n1, blasint* n2, float* q, blasint* ldq, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(sorm22,SORM22)((void*) side, (void*) trans, (void*) m, (void*) n, (void*) n1, (void*) n2, (void*) q, (void*) ldq, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sorm22_(void* side, void* trans, void* m, void* n, void* n1, void* n2, void* q, void* ldq, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
{
	void (*fn) (void* side, void* trans, void* m, void* n, void* n1, void* n2, void* q, void* ldq, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

	*(void **) & fn = current_backend->lapack.sorm22.f77_blas_function; 

		fn((void*) side, (void*) trans, (void*) m, (void*) n, (void*) n1, (void*) n2, (void*) q, (void*) ldq, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_sorm22(void* side, void* trans, void* m, void* n, void* n1, void* n2, void* q, void* ldq, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_sorm22_")));
#else
void flexiblas_real_sorm22(void* side, void* trans, void* m, void* n, void* n1, void* n2, void* q, void* ldq, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){flexiblas_real_sorm22_((void*) side, (void*) trans, (void*) m, (void*) n, (void*) n1, (void*) n2, (void*) q, (void*) ldq, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sorm22_(void* side, void* trans, void* m, void* n, void* n1, void* n2, void* q, void* ldq, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
{
	void (*fn) (void* side, void* trans, void* m, void* n, void* n1, void* n2, void* q, void* ldq, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);
	void (*fn_hook) (void* side, void* trans, void* m, void* n, void* n1, void* n2, void* q, void* ldq, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

	*(void **) &fn      = current_backend->lapack.sorm22.f77_blas_function; 

    hook_pos_sorm22 ++;
    if( hook_pos_sorm22 < __flexiblas_hooks->sorm22.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sorm22.f77_hook_function[hook_pos_sorm22];
        fn_hook((void*) side, (void*) trans, (void*) m, (void*) n, (void*) n1, (void*) n2, (void*) q, (void*) ldq, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans);
    } else {
        hook_pos_sorm22 = 0;
		fn((void*) side, (void*) trans, (void*) m, (void*) n, (void*) n1, (void*) n2, (void*) q, (void*) ldq, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_sorm22(void* side, void* trans, void* m, void* n, void* n1, void* n2, void* q, void* ldq, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_sorm22_")));
#else
void flexiblas_chain_sorm22(void* side, void* trans, void* m, void* n, void* n1, void* n2, void* q, void* ldq, void* c, void* ldc, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){flexiblas_chain_sorm22_((void*) side, (void*) trans, (void*) m, (void*) n, (void*) n1, (void*) n2, (void*) q, (void*) ldq, (void*) c, (void*) ldc, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans);}
#endif



