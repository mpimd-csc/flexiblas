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


static TLS_STORE uint8_t hook_pos_ztpmlqt = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ztpmlqt,ZTPMLQT)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, blasint* mb, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
#else
void FC_GLOBAL(ztpmlqt,ZTPMLQT)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, blasint* mb, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
#endif
{
	void (*fn) (void* side, void* trans, void* m, void* n, void* k, void* l, void* mb, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);
	void (*fn_hook) (void* side, void* trans, void* m, void* n, void* k, void* l, void* mb, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ztpmlqt.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ztpmlqt.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) l, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans); 
		return;
	} else {
		hook_pos_ztpmlqt = 0;
		fn_hook((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) l, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ztpmlqt_(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, blasint* mb, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(ztpmlqt,ZTPMLQT)))));
#else
#ifndef __APPLE__
void ztpmlqt(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, blasint* mb, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(ztpmlqt,ZTPMLQT)))));
#else
void ztpmlqt(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, blasint* mb, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){ FC_GLOBAL(ztpmlqt,ZTPMLQT)((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) l, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ztpmlqt_(void* side, void* trans, void* m, void* n, void* k, void* l, void* mb, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
{
	void (*fn) (void* side, void* trans, void* m, void* n, void* k, void* l, void* mb, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

	*(void **) & fn = current_backend->lapack.ztpmlqt.f77_blas_function; 

		fn((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) l, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_ztpmlqt(void* side, void* trans, void* m, void* n, void* k, void* l, void* mb, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_ztpmlqt_")));
#else
void flexiblas_real_ztpmlqt(void* side, void* trans, void* m, void* n, void* k, void* l, void* mb, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){flexiblas_real_ztpmlqt_((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) l, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ztpmlqt_(void* side, void* trans, void* m, void* n, void* k, void* l, void* mb, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans)
{
	void (*fn) (void* side, void* trans, void* m, void* n, void* k, void* l, void* mb, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);
	void (*fn_hook) (void* side, void* trans, void* m, void* n, void* k, void* l, void* mb, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

	*(void **) &fn      = current_backend->lapack.ztpmlqt.f77_blas_function; 

    hook_pos_ztpmlqt ++;
    if( hook_pos_ztpmlqt < __flexiblas_hooks->ztpmlqt.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ztpmlqt.f77_hook_function[hook_pos_ztpmlqt];
        fn_hook((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) l, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans);
    } else {
        hook_pos_ztpmlqt = 0;
		fn((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) l, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_trans); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_ztpmlqt(void* side, void* trans, void* m, void* n, void* k, void* l, void* mb, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_ztpmlqt_")));
#else
void flexiblas_chain_ztpmlqt(void* side, void* trans, void* m, void* n, void* k, void* l, void* mb, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans){flexiblas_chain_ztpmlqt_((void*) side, (void*) trans, (void*) m, (void*) n, (void*) k, (void*) l, (void*) mb, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_trans);}
#endif



