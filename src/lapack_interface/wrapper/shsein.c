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



static TLS_STORE uint8_t hook_pos_shsein = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(shsein,SHSEIN)(char* side, char* eigsrc, char* initv, blasint* selectfunc, blasint* n, float* h, blasint* ldh, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* ifaill, blasint* ifailr, blasint* info)
#else
void FC_GLOBAL(shsein,SHSEIN)(char* side, char* eigsrc, char* initv, blasint* selectfunc, blasint* n, float* h, blasint* ldh, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* ifaill, blasint* ifailr, blasint* info)
#endif
{
	void (*fn) (void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info);
	void (*fn_hook) (void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.shsein.f77_blas_function; 
	fn_hook = __flexiblas_hooks->shsein.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) selectfunc, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info); 
		return;
	} else {
		hook_pos_shsein = 0;
		fn_hook((void*) side, (void*) eigsrc, (void*) initv, (void*) selectfunc, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void shsein_(char* side, char* eigsrc, char* initv, blasint* selectfunc, blasint* n, float* h, blasint* ldh, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* ifaill, blasint* ifailr, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(shsein,SHSEIN)))));
#else
void shsein(char* side, char* eigsrc, char* initv, blasint* selectfunc, blasint* n, float* h, blasint* ldh, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* ifaill, blasint* ifailr, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(shsein,SHSEIN)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_shsein_(void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info)
{
	void (*fn) (void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info);

	fn = current_backend->lapack.shsein.f77_blas_function; 

		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) selectfunc, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info); 

	return;
}

void flexiblas_real_shsein(void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info)  __attribute__((alias("flexiblas_real_shsein_")));





/* Chainloader for Hooks */


void flexiblas_chain_shsein_(void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info)
{
	void (*fn) (void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info);
	void (*fn_hook) (void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info);

	fn      = current_backend->lapack.shsein.f77_blas_function; 

    hook_pos_shsein ++;
    if( hook_pos_shsein < __flexiblas_hooks->shsein.nhook) {
        fn_hook = __flexiblas_hooks->shsein.f77_hook_function[hook_pos_shsein];
        fn_hook((void*) side, (void*) eigsrc, (void*) initv, (void*) selectfunc, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info);
    } else {
        hook_pos_shsein = 0;
		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) selectfunc, (void*) n, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) ifaill, (void*) ifailr, (void*) info); 
	}
	return;
}

void flexiblas_chain_shsein(void* side, void* eigsrc, void* initv, void* selectfunc, void* n, void* h, void* ldh, void* wr, void* wi, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* ifaill, void* ifailr, void* info)  __attribute__((alias("flexiblas_chain_shsein_")));




