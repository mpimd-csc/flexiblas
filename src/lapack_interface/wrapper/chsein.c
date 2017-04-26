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
 * Copyright (C) Martin Koehler, 2015-2017
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Tue Mar 28 16:07:33 2017 */ 
        
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



#ifdef FLEXIBLAS_ABI_INTEL 
void FC_GLOBAL(chsein,CHSEIN)(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, float* rwork, blasint* ifaill, blasint* ifailr, blasint* info)
#else
void FC_GLOBAL(chsein,CHSEIN)(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, float* rwork, blasint* ifaill, blasint* ifailr, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* side, void* eigsrc, void* initv, void* select, void* n, void* h, void* ldh, void* w, void* vl, void* ldvl, void* vr, void* ldvr, void* mm, void* m, void* work, void* rwork, void* ifaill, void* ifailr, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.chsein.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info); 
		current_backend->lapack.chsein.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.chsein.calls[0]++;
	} else { 
		fn((void*) side, (void*) eigsrc, (void*) initv, (void*) select, (void*) n, (void*) h, (void*) ldh, (void*) w, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) mm, (void*) m, (void*) work, (void*) rwork, (void*) ifaill, (void*) ifailr, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void chsein_(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, float* rwork, blasint* ifaill, blasint* ifailr, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(chsein,CHSEIN)))));
#else
void chsein(char* side, char* eigsrc, char* initv, blasint* select, blasint* n, float complex* h, blasint* ldh, float complex* w, float complex* vl, blasint* ldvl, float complex* vr, blasint* ldvr, blasint* mm, blasint* m, float complex* work, float* rwork, blasint* ifaill, blasint* ifailr, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(chsein,CHSEIN)))));
#endif



