/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Wed Mar 28 11:20:04 2018 */
        
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "fortran_mangle.h"

#include "flexiblas.h"


#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_dtrevc = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dtrevc,DTREVC)(char* side, char* howmny, blasint* selectfunc, blasint* n, double* t, blasint* ldt, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* info)
#else
void FC_GLOBAL(dtrevc,DTREVC)(char* side, char* howmny, blasint* selectfunc, blasint* n, double* t, blasint* ldt, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* info)
#endif
{
	void (*fn) (void* side, void* howmny, void* selectfunc, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info);
	void (*fn_hook) (void* side, void* howmny, void* selectfunc, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dtrevc.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dtrevc.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) howmny, (void*) selectfunc, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info); 
		return;
	} else {
		hook_pos_dtrevc = 0;
		fn_hook((void*) side, (void*) howmny, (void*) selectfunc, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dtrevc_(char* side, char* howmny, blasint* selectfunc, blasint* n, double* t, blasint* ldt, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dtrevc,DTREVC)))));
#else
void dtrevc(char* side, char* howmny, blasint* selectfunc, blasint* n, double* t, blasint* ldt, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dtrevc,DTREVC)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dtrevc_(void* side, void* howmny, void* selectfunc, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info)
{
	void (*fn) (void* side, void* howmny, void* selectfunc, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info);

	fn = current_backend->lapack.dtrevc.f77_blas_function; 

		fn((void*) side, (void*) howmny, (void*) selectfunc, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info); 

	return;
}

void flexiblas_real_dtrevc(void* side, void* howmny, void* selectfunc, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info)  __attribute__((alias("flexiblas_real_dtrevc_")));





/* Chainloader for Hooks */


void flexiblas_chain_dtrevc_(void* side, void* howmny, void* selectfunc, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info)
{
	void (*fn) (void* side, void* howmny, void* selectfunc, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info);
	void (*fn_hook) (void* side, void* howmny, void* selectfunc, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info);

	fn      = current_backend->lapack.dtrevc.f77_blas_function; 

    hook_pos_dtrevc ++;
    if( hook_pos_dtrevc < __flexiblas_hooks->dtrevc.nhook) {
        fn_hook = __flexiblas_hooks->dtrevc.f77_hook_function[hook_pos_dtrevc];
        fn_hook((void*) side, (void*) howmny, (void*) selectfunc, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info);
    } else {
        hook_pos_dtrevc = 0;
		fn((void*) side, (void*) howmny, (void*) selectfunc, (void*) n, (void*) t, (void*) ldt, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) info); 
	}
	return;
}

void flexiblas_chain_dtrevc(void* side, void* howmny, void* selectfunc, void* n, void* t, void* ldt, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* info)  __attribute__((alias("flexiblas_chain_dtrevc_")));




