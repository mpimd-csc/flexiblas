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


static TLS_STORE uint8_t hook_pos_ctrsna = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ctrsna,CTRSNA)(char* job, char* howmny, blasint* select, blasint* n, float complex* t, blasint* ldt, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, float* s, float* sep, blasint* mm, blasint* m, float complex* work, blasint* ldwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny)
#else
void FC_GLOBAL(ctrsna,CTRSNA)(char* job, char* howmny, blasint* select, blasint* n, float complex* t, blasint* ldt, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, float* s, float* sep, blasint* mm, blasint* m, float complex* work, blasint* ldwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny)
#endif
{
	void (*fn) (void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);
	void (*fn_hook) (void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ctrsna.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ctrsna.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny); 
		return;
	} else {
		hook_pos_ctrsna = 0;
		fn_hook((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ctrsna_(char* job, char* howmny, blasint* select, blasint* n, float complex* t, blasint* ldt, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, float* s, float* sep, blasint* mm, blasint* m, float complex* work, blasint* ldwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias(MTS(FC_GLOBAL(ctrsna,CTRSNA)))));
#else
#ifndef __APPLE__
void ctrsna(char* job, char* howmny, blasint* select, blasint* n, float complex* t, blasint* ldt, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, float* s, float* sep, blasint* mm, blasint* m, float complex* work, blasint* ldwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias(MTS(FC_GLOBAL(ctrsna,CTRSNA)))));
#else
void ctrsna(char* job, char* howmny, blasint* select, blasint* n, float complex* t, blasint* ldt, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, float* s, float* sep, blasint* mm, blasint* m, float complex* work, blasint* ldwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny){ FC_GLOBAL(ctrsna,CTRSNA)((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_howmny); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ctrsna_(void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny)
{
	void (*fn) (void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

	*(void **) & fn = current_backend->lapack.ctrsna.f77_blas_function; 

		fn((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_ctrsna(void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias("flexiblas_real_ctrsna_")));
#else
void flexiblas_real_ctrsna(void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny){flexiblas_real_ctrsna_((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_howmny);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ctrsna_(void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny)
{
	void (*fn) (void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);
	void (*fn_hook) (void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

	*(void **) &fn      = current_backend->lapack.ctrsna.f77_blas_function; 

    hook_pos_ctrsna ++;
    if( hook_pos_ctrsna < __flexiblas_hooks->ctrsna.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ctrsna.f77_hook_function[hook_pos_ctrsna];
        fn_hook((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny);
    } else {
        hook_pos_ctrsna = 0;
		fn((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_ctrsna(void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias("flexiblas_chain_ctrsna_")));
#else
void flexiblas_chain_ctrsna(void* job, void* howmny, void* select, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* sep, void* mm, void* m, void* work, void* ldwork, void* rwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny){flexiblas_chain_ctrsna_((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) sep, (void*) mm, (void*) m, (void*) work, (void*) ldwork, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_howmny);}
#endif



