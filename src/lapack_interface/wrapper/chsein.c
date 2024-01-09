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


#if __GNUC__ > 7
typedef size_t fortran_charlen_t;
#else
typedef int fortran_charlen_t;
#endif

#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_chsein = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(chsein,CHSEIN)(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, float* rwork, blasint* ifaill, blasint* ifailr, blasint* info)
#else
void FC_GLOBAL(chsein,CHSEIN)(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, float* rwork, blasint* ifaill, blasint* ifailr, blasint* info)
#endif
{
	void (*fn) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info);
	void (*fn_hook) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.chsein.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->chsein.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info); 
		return;
	} else {
		hook_pos_chsein = 0;
		fn_hook((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void chsein_(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, float* rwork, blasint* ifaill, blasint* ifailr, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(chsein,CHSEIN)))));
#else
#ifndef __APPLE__
void chsein(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, float* rwork, blasint* ifaill, blasint* ifailr, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(chsein,CHSEIN)))));
#else
void chsein(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, float* rwork, blasint* ifaill, blasint* ifailr, blasint* info){ FC_GLOBAL(chsein,CHSEIN)((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_chsein_(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info)
{
	void (*fn) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info);

	*(void **) & fn = current_backend->lapack.chsein.f77_blas_function; 

		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_chsein(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info) __attribute__((alias("flexiblas_real_chsein_")));
#else
void flexiblas_real_chsein(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info){flexiblas_real_chsein_((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_chsein_(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info)
{
	void (*fn) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info);
	void (*fn_hook) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info);

	*(void **) &fn      = current_backend->lapack.chsein.f77_blas_function; 

    hook_pos_chsein ++;
    if( hook_pos_chsein < __flexiblas_hooks->chsein.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->chsein.f77_hook_function[hook_pos_chsein];
        fn_hook((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info);
    } else {
        hook_pos_chsein = 0;
		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_chsein(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info) __attribute__((alias("flexiblas_chain_chsein_")));
#else
void flexiblas_chain_chsein(void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info){flexiblas_chain_chsein_((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info);}
#endif



