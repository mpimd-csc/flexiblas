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
 /* Generated: Wed Mar 28 11:20:03 2018 */
        
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



static TLS_STORE uint8_t hook_pos_dhsein = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dhsein,DHSEIN)(char* side, char* eigsrc, char* initv, blasint* selectfunc, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* ifaill, blasint* ifailr, blasint* info)
#else
void FC_GLOBAL(dhsein,DHSEIN)(char* side, char* eigsrc, char* initv, blasint* selectfunc, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* ifaill, blasint* ifailr, blasint* info)
#endif
{
	void (*fn) (void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info);
	void (*fn_hook) (void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dhsein.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dhsein.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) selectfunc, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info); 
		return;
	} else {
		hook_pos_dhsein = 0;
		fn_hook((void*) side, (void*) eigsrc, (void*) initv, (void*) selectfunc, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dhsein_(char* side, char* eigsrc, char* initv, blasint* selectfunc, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* ifaill, blasint* ifailr, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dhsein,DHSEIN)))));
#else
void dhsein(char* side, char* eigsrc, char* initv, blasint* selectfunc, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* ifaill, blasint* ifailr, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dhsein,DHSEIN)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dhsein_(void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info)
{
	void (*fn) (void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info);

	fn = current_backend->lapack.dhsein.f77_blas_function; 

		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) selectfunc, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info); 

	return;
}

void flexiblas_real_dhsein(void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info)  __attribute__((alias("flexiblas_real_dhsein_")));





/* Chainloader for Hooks */


void flexiblas_chain_dhsein_(void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info)
{
	void (*fn) (void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info);
	void (*fn_hook) (void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info);

	fn      = current_backend->lapack.dhsein.f77_blas_function; 

    hook_pos_dhsein ++;
    if( hook_pos_dhsein < __flexiblas_hooks->dhsein.nhook) {
        fn_hook = __flexiblas_hooks->dhsein.f77_hook_function[hook_pos_dhsein];
        fn_hook((void*) side, (void*) eigsrc, (void*) initv, (void*) selectfunc, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info);
    } else {
        hook_pos_dhsein = 0;
		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) selectfunc, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info); 
	}
	return;
}

void flexiblas_chain_dhsein(void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info)  __attribute__((alias("flexiblas_chain_dhsein_")));




