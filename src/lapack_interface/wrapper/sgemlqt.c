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


static TLS_STORE uint8_t hook_pos_sgemlqt = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgemlqt,SGEMLQT)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* mb, float* v, blasint* ldv, float* t, blasint* ldt, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
#else
void FC_GLOBAL(sgemlqt,SGEMLQT)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* mb, float* v, blasint* ldv, float* t, blasint* ldt, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
#endif
{
	void (*fn) (void* side, void* trans, void* m, void* n, void* k, void* mb, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);
	void (*fn_hook) (void* side, void* trans, void* m, void* n, void* k, void* mb, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.sgemlqt.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->sgemlqt.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans); 
		return;
	} else {
		hook_pos_sgemlqt = 0;
		fn_hook((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sgemlqt_(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* mb, float* v, blasint* ldv, float* t, blasint* ldt, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(sgemlqt,SGEMLQT)))));
#else
#ifndef __APPLE__
void sgemlqt(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* mb, float* v, blasint* ldv, float* t, blasint* ldt, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(sgemlqt,SGEMLQT)))));
#else
void sgemlqt(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* mb, float* v, blasint* ldv, float* t, blasint* ldt, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(sgemlqt,SGEMLQT)((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgemlqt_(void* side, void* trans, void* m, void* n, void* k, void* mb, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
{
	void (*fn) (void* side, void* trans, void* m, void* n, void* k, void* mb, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

	*(void **) & fn = current_backend->lapack.sgemlqt.f77_blas_function; 

		fn((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_sgemlqt(void* side, void* trans, void* m, void* n, void* k, void* mb, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_sgemlqt_")));
#else
void flexiblas_real_sgemlqt(void* side, void* trans, void* m, void* n, void* k, void* mb, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){flexiblas_real_sgemlqt_((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sgemlqt_(void* side, void* trans, void* m, void* n, void* k, void* mb, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
{
	void (*fn) (void* side, void* trans, void* m, void* n, void* k, void* mb, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);
	void (*fn_hook) (void* side, void* trans, void* m, void* n, void* k, void* mb, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

	*(void **) &fn      = current_backend->lapack.sgemlqt.f77_blas_function; 

    hook_pos_sgemlqt ++;
    if( hook_pos_sgemlqt < __flexiblas_hooks->sgemlqt.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sgemlqt.f77_hook_function[hook_pos_sgemlqt];
        fn_hook((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans);
    } else {
        hook_pos_sgemlqt = 0;
		fn((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_sgemlqt(void* side, void* trans, void* m, void* n, void* k, void* mb, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_sgemlqt_")));
#else
void flexiblas_chain_sgemlqt(void* side, void* trans, void* m, void* n, void* k, void* mb, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){flexiblas_chain_sgemlqt_((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans);}
#endif



