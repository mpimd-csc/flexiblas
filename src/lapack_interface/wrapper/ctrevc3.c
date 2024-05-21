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


static TLS_STORE uint8_t hook_pos_ctrevc3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ctrevc3,CTREVC3)(char* side, char* howmny, blasint* select, blasint* n, float complex* t, blasint* ldt, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny)
#else
void FC_GLOBAL(ctrevc3,CTREVC3)(char* side, char* howmny, blasint* select, blasint* n, float complex* t, blasint* ldt, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny)
#endif
{
	void (*fn) (void* side, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);
	void (*fn_hook) (void* side, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ctrevc3.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ctrevc3.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_howmny); 
		return;
	} else {
		hook_pos_ctrevc3 = 0;
		fn_hook((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_howmny);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ctrevc3_(char* side, char* howmny, blasint* select, blasint* n, float complex* t, blasint* ldt, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias(MTS(FC_GLOBAL(ctrevc3,CTREVC3)))));
#else
#ifndef __APPLE__
void ctrevc3(char* side, char* howmny, blasint* select, blasint* n, float complex* t, blasint* ldt, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias(MTS(FC_GLOBAL(ctrevc3,CTREVC3)))));
#else
void ctrevc3(char* side, char* howmny, blasint* select, blasint* n, float complex* t, blasint* ldt, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny){ FC_GLOBAL(ctrevc3,CTREVC3)((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_howmny); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ctrevc3_(void* side, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny)
{
	void (*fn) (void* side, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

	*(void **) & fn = current_backend->lapack.ctrevc3.f77_blas_function; 

		fn((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_howmny); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_ctrevc3(void* side, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias("flexiblas_real_ctrevc3_")));
#else
void flexiblas_real_ctrevc3(void* side, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny){flexiblas_real_ctrevc3_((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_howmny);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ctrevc3_(void* side, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny)
{
	void (*fn) (void* side, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);
	void (*fn_hook) (void* side, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

	*(void **) &fn      = current_backend->lapack.ctrevc3.f77_blas_function; 

    hook_pos_ctrevc3 ++;
    if( hook_pos_ctrevc3 < __flexiblas_hooks->ctrevc3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ctrevc3.f77_hook_function[hook_pos_ctrevc3];
        fn_hook((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_howmny);
    } else {
        hook_pos_ctrevc3 = 0;
		fn((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_side, ( flexiblas_fortran_charlen_t ) len_howmny); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_ctrevc3(void* side, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias("flexiblas_chain_ctrevc3_")));
#else
void flexiblas_chain_ctrevc3(void* side, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* lwork, void* rwork, void* lrwork, void* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny){flexiblas_chain_ctrevc3_((void*) side, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info, (flexiblas_fortran_charlen_t) len_side, (flexiblas_fortran_charlen_t) len_howmny);}
#endif



