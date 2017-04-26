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
 /* Generated: Tue Mar 28 16:07:36 2017 */ 
        
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
void FC_GLOBAL(slarrd,SLARRD)(char* range, char* order, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* gers, float* reltol, float* d, float* e, float* e2, float* pivmin, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wl, float* wu, blasint* iblock, blasint* indexw, float* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(slarrd,SLARRD)(char* range, char* order, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* gers, float* reltol, float* d, float* e, float* e2, float* pivmin, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wl, float* wu, blasint* iblock, blasint* indexw, float* work, blasint* iwork, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* range, void* order, void* n, void* vl, void* vu, void* il, void* iu, void* gers, void* reltol, void* d, void* e, void* e2, void* pivmin, void* nsplit, void* isplit, void* m, void* w, void* werr, void* wl, void* wu, void* iblock, void* indexw, void* work, void* iwork, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.slarrd.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info); 
		current_backend->lapack.slarrd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.slarrd.calls[0]++;
	} else { 
		fn((void*) range, (void*) order, (void*) n, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) gers, (void*) reltol, (void*) d, (void*) e, (void*) e2, (void*) pivmin, (void*) nsplit, (void*) isplit, (void*) m, (void*) w, (void*) werr, (void*) wl, (void*) wu, (void*) iblock, (void*) indexw, (void*) work, (void*) iwork, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void slarrd_(char* range, char* order, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* gers, float* reltol, float* d, float* e, float* e2, float* pivmin, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wl, float* wu, blasint* iblock, blasint* indexw, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slarrd,SLARRD)))));
#else
void slarrd(char* range, char* order, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* gers, float* reltol, float* d, float* e, float* e2, float* pivmin, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wl, float* wu, blasint* iblock, blasint* indexw, float* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slarrd,SLARRD)))));
#endif



